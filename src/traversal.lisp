(in-package :cl-json-pointer)

;;; Switches

(defvar *traverse-treat-string-as-atom* t
  ;; I don't want to treat string as an array.
  "If this is T, cl-json-pointer trests string as atom.")

(defvar *traverse-nil-set-to-last-method* :list
  "Determines how to set to the last (by '-') of NIL.
- `:list' :: (Default) pushes <value> as an ordinal list.
- `:alist' :: pushes (reference-token . <value>) as an alist.
- `:plist' :: appends (reference-token <value>) as an plist.
- `:array' :: makes a new array contains <value>.")
(declaim (type symbol *traverse-nil-set-to-last-method*))

(defvar *traverse-nil-set-to-index-method* :list
  "Determines how to set to NIL by an index.
- `:list' :: (Default) makes a new list and set <value> into nth point.
- `:alist' :: pushes (reference-token . <value>) as an alist.
- `:plist' :: appends (reference-token <value>) as an plist.
- `:error' :: throws an error.")
(declaim (type symbol *traverse-nil-set-to-index-method*))

(defvar *traverse-nil-set-to-name-method* :alist
  "Determines how to set to NIL by a name.
- `:alist' :: (Default) pushes (reference-token . <value>) as an alist.
- `:plist' :: appends (reference-token <value>) as an plist.")
(declaim (type symbol *traverse-nil-set-to-name-method*))

(defvar *traverse-object-like-kinds* '(:alist :plist))

;;; Tools

(defmacro chained-setter-lambda
    ((&rest vars) (next-function &optional (next-arg nil na-supplied-p)) &body body)
  `(lambda (,@vars)
     (funcall ,next-function
	      (progn ,@body
		     ,@(if na-supplied-p `(,next-arg) ())))))

;;; Main traversal.

(defgeneric traverse-by-reference-token (flavor obj rtoken set-method next-setter)
  (:documentation "Traverses OBJ with a reference token (RTOKEN), and
returns three values: a referred object, existence (boolean), and a
closure can be used as a setter.

FLAVOR is used when OBJ's type is ambiguous, especially lists."))

(defun bad-deleter-error (obj rtoken)
  (error 'json-pointer-access-error
	 :format-control "Object ~S's point ~A is not a place to delete"
	 :format-arguments (list obj rtoken)))

;;; Atoms

(defun error-on-traversing-atom (flavor obj rtoken)
  ;; FIXME: I think this should be error, but a kind of 'silent' option is required..
  (values nil nil
	  (thunk-lambda
	    (error 'json-pointer-access-error
		   :format-control "Obj ~S is considered as atom in cl-json-pointer and not is not allowed to traverse (reference token: ~A, flavor: ~A)"
		   :format-arguments (list obj rtoken flavor)))))

(defmethod traverse-by-reference-token (flavor obj rtoken set-method next-setter)
  (declare (ignore set-method next-setter))
  ;; bottom case -- refers an unsupported object.
  (error-on-traversing-atom obj flavor rtoken))

(defmethod traverse-by-reference-token (flavor (obj string) rtoken set-method next-setter)
  (declare (ignore set-method next-setter))
  (if *traverse-treat-string-as-atom*
      (error-on-traversing-atom obj flavor rtoken)
      (call-next-method)))

;;; List

(defmethod traverse-by-reference-token ((flavor (eql :alist)) (alist list) rtoken
					set-method next-setter)
  ;; accepts `nil' as alist.
  (flet ((add-to-head (x)
	   (acons rtoken x alist))) ; assumes RTOKEN is interned.
    (if-let ((entry (assoc rtoken alist :test #'compare-string-by-readtable-case)))
      (values (cdr entry) entry
	      (ecase set-method
		((nil) nil)
		(:update
		 (chained-setter-lambda (x) (next-setter alist)
		   (setf (cdr entry) x)))
		(:add
		 (chained-setter-lambda (x) (next-setter)
		   (add-to-head x)))
		(:delete
		 (chained-setter-lambda () (next-setter)
		   (delete entry alist)))
		(:remove
		 (chained-setter-lambda () (next-setter)
		   (remove entry alist)))))
      (values nil nil
	      (ecase set-method
		((nil) nil)
		((:add :update)
		 (chained-setter-lambda (x) (next-setter)
		   (add-to-head x)))
		((:delete :remove)
		 (thunk-lambda
		   (bad-deleter-error alist rtoken))))))))

(defmethod traverse-by-reference-token ((flavor (eql :plist)) (plist list) rtoken
					set-method next-setter)
  ;; accepts `nil' as plist.
  (flet ((add-to-head (x)
	   (list* rtoken x plist))) ; assumes RTOKEN is interned.
    (loop for plist-head on plist by #'cddr
       as (k v) = plist-head
       when (compare-string-by-readtable-case k rtoken) ; plist often uses `eq', but I use this.
       return (values v plist-head
		      (ecase set-method
			((nil) nil)
			(:update
			 (chained-setter-lambda (x) (next-setter plist)
			   (setf (cadr plist-head) x)))
			(:add
			 (chained-setter-lambda (x) (next-setter)
			   (add-to-head x)))
			(:delete
			 (chained-setter-lambda () (next-setter)
			   (delete-cons plist plist-head 2)))
			(:remove
			 (chained-setter-lambda () (next-setter)
			   (remove-cons plist plist-head 2)))))
       finally
	 (return (values nil nil
			 (ecase set-method
			   ((nil) nil)
			   ((:update :add)
			    (chained-setter-lambda (x) (next-setter)
			      (add-to-head x)))
			   ((:delete :remove)
			    (thunk-lambda
			      (bad-deleter-error plist rtoken)))))))))
		       
(defmethod traverse-by-reference-token ((flavor (eql :list)) (list list)
					(rtoken (eql +end+)) set-method next-setter)
  "Pushing to an ordinal list."
  (values nil nil
	  (ecase set-method
	    ((nil) nil)
	    (:update
	     (chained-setter-lambda (x) (next-setter)
	       (nconc list (list x))))
	    (:add
	     (chained-setter-lambda (x) (next-setter)
	       (append list (list x))))
	    ((:delete :remove)
	     (thunk-lambda
	       (bad-deleter-error list rtoken))))))
	
(defmethod traverse-by-reference-token ((flavor (eql :list)) (list list)
					(index integer) set-method next-setter)
  "Indexing to an ordinal list."
  (if-let ((this-cons (nthcdr index list)))
    (values (car this-cons) this-cons
	    (ecase set-method
	      ((nil) nil)
	      (:update
	       (chained-setter-lambda (x) (next-setter list)
		 (setf (car this-cons) x)))
	      (:add
	       (chained-setter-lambda (x) (next-setter)
		 (clone-and-replace-on-cons list this-cons x)))
	      (:delete
	       (chained-setter-lambda () (next-setter)
		 (delete-cons list this-cons)))
	      (:remove
	       (chained-setter-lambda () (next-setter)
		 (remove-cons list this-cons)))))
    (values nil nil
	    (ecase set-method
	      ((nil) nil)
	      ((:delete :remove)
	       (thunk-lambda
		 (bad-deleter-error list index)))
	      ;; These cases works, but confusing with `alist-like-p'...
	      (:update
	       (chained-setter-lambda (x) (next-setter list)
		 ;; TODO: should be more efficient..
		 (setf list (extend-list list (1+ index)))
		 (setf (nth index list) x)))
	      (:add
	       (chained-setter-lambda (x) (next-setter list)
		 ;; TODO: should be more efficient..
		 (setf list (extend-list (copy-list list) (1+ index)))
		 (setf (nth index list) x)))))))

(defmethod traverse-by-reference-token ((flavor (eql :list)) (list list)
					(rtoken string) set-method next-setter)
  "Indexing to an ordinal list."
  (traverse-by-reference-token flavor list
			       (read-reference-token-as-index rtoken)
			       set-method next-setter))

(defmethod traverse-by-reference-token (flavor (obj list) (rtoken (eql +end+)) set-method next-setter)
  (declare (ignore flavor))
  (traverse-by-reference-token :list obj rtoken set-method next-setter))

(defmethod traverse-by-reference-token (flavor (obj list) (rtoken integer) set-method next-setter)
  (declare (ignore flavor))
  (traverse-by-reference-token :list obj rtoken set-method next-setter))

(defun list-try-traverse (kinds flavor list rtoken set-method next-setter)
  ;; RTOKEN may be ambiguous with an index or a name of object fields.
  ;; 
  ;; 1. Try to use it as an existed field name.
  ;; FIXME: This loop is too heavy! (but I think this is required..)
  (let* ((set-to-nil-kind-default nil)
	 (interned-rtoken (intern-object-key flavor rtoken))
	 (try-results
	  (loop for kind in kinds
	     as ret =
	       (handler-case
		   (multiple-value-list
		    (traverse-by-reference-token kind list interned-rtoken set-method next-setter))
		 (error () nil))
	     if (second ret)		; exists?
	     do (return-from list-try-traverse
		  (values-list ret))
	     else if (eq kind *traverse-nil-set-to-name-method*)
	     do (setf set-to-nil-kind-default ret) ; See '3-1.' below.
	     else
	     collect ret)))
    ;; RTOKEN is not a name of object fields.
    ;;
    ;; 2. If it can be read as an index, I treat OBJ as an ordinal list.
    (multiple-value-bind (index bad-index-condition)
	(read-reference-token-as-index rtoken nil)
      (when index			; yes, an ordinal list!
	(return-from list-try-traverse
	  (traverse-by-reference-token :list list rtoken set-method next-setter)))
      (when (typep bad-index-condition 'json-pointer-bad-reference-token-0-used-error)
	(error bad-index-condition)))
    ;; 3. RTOKEN assumed as a field name, but not found in the list.
    ;; 3-1. use the specified default.
    (when (and set-to-nil-kind-default
	       (third set-to-nil-kind-default))
      (return-from list-try-traverse
	(values-list set-to-nil-kind-default)))
    ;; 3-2. use a found one.
    (loop for ret in try-results
       when (third ret)
       return (return-from list-try-traverse
		(values-list ret)))
    ;; 3-3. no way...
    (values nil nil
	    (if set-method
		(thunk-lambda
		  (error 'json-pointer-access-error
			 :format-control "There is no way to set to ~A (rtoken ~A)"
			 :format-arguments (list list rtoken)))))))

(defmethod traverse-by-reference-token (flavor (obj list) (rtoken string) set-method next-setter)
  (list-try-traverse *traverse-object-like-kinds* flavor
		     obj rtoken set-method next-setter))

(defmethod traverse-by-reference-token (flavor (obj null) rtoken set-method next-setter)
  ;; empty. this is problematic for setting.
  (values nil nil
	  (ecase set-method
	    ((nil) nil)
	    ((:delete :remove)
	     (thunk-lambda
	       (bad-deleter-error obj rtoken)))
	    ((:update :add)
	     (let* ((index (read-reference-token-as-index rtoken nil))
		    (nil-method
		     (cond ((eq index +end+)
			    *traverse-nil-set-to-last-method*)
			   ((integerp index)
			    *traverse-nil-set-to-index-method*)
			   (t
			    *traverse-nil-set-to-name-method*))))
	       (ecase nil-method
		 ((:alist :plist)
		  (nth-value 2 (traverse-by-reference-token
				nil-method obj
				(intern-object-key flavor rtoken)
				set-method next-setter)))
		 (:list
		  (nth-value 2 (traverse-by-reference-token
				nil-method obj rtoken set-method next-setter)))
		 (:array
		  (chained-setter-lambda (x) (next-setter)
		    (make-array 1 :adjustable t :initial-element x :fill-pointer t)))
		 (:error
		  (thunk-lambda
		    (error 'json-pointer-access-error
			   :format-control "Set to nil by '~A' is not supported" index)))))))))

;;; Objects

(defun traverse-by-reference-token-using-class (flavor obj rtoken set-method next-setter class)
  (let ((object-key (intern-object-key flavor rtoken)))
    (if-let ((slot (find object-key (class-slots class)
			 :key #'slot-definition-name
			 :test #'string=)))
      (let ((bound? (slot-boundp-using-class class obj slot)))
	(values (if bound?
		    (slot-value-using-class class obj slot))
		bound?
		(ecase set-method
		  ((nil) nil)
		  ((:update :add)
		   (chained-setter-lambda (x) (next-setter obj)
		     (setf (slot-value-using-class class obj slot) x)))
		  ((:remove :delete)
		   (if bound?
		       (chained-setter-lambda () (next-setter obj)
			 (slot-makunbound-using-class class obj slot))
		       (thunk-lambda
			 (error 'json-pointer-access-error
				:format-control "object ~A's '~A' slot is unbound"
				:format-arguments (list obj rtoken))))))))
      (values nil nil
	      (if set-method	     ; TODO: add slot? (only if "add")
		  (thunk-lambda
		    (error 'json-pointer-access-error
			   :format-control "object ~A does not have '~A' slot"
			   :format-arguments (list obj rtoken))))))))

(defmethod traverse-by-reference-token (flavor (obj standard-object) rtoken set-method next-setter)
  ;; `cl-json:fluid-object' can be treated here.
  (traverse-by-reference-token-using-class flavor obj rtoken set-method next-setter (class-of obj)))

(defmethod traverse-by-reference-token (flavor (obj structure-object) rtoken set-method next-setter)
  (traverse-by-reference-token-using-class flavor obj rtoken set-method next-setter (class-of obj)))

;;; Hash table

(defmethod traverse-by-reference-token (flavor (obj hash-table) rtoken set-method next-setter)
  (let ((object-key (intern-object-key flavor rtoken)))
    (multiple-value-bind (value exists?) (gethash object-key obj)
      (values value exists?
	      (ecase set-method
		((nil) nil)
		((:add :update) 
		 (chained-setter-lambda (x) (next-setter obj)
		   (setf (gethash object-key obj) x)))
		((:delete :remove)
		 (if exists?
		     (chained-setter-lambda () (next-setter obj)
		       (remhash object-key obj))
		     (thunk-lambda
		       (error 'json-pointer-access-error
			      :format-control "Hash-table ~A does not have ~A key"
			      :format-arguments (list obj rtoken))))))))))

;;; Array

(defmethod traverse-by-reference-token (flavor (obj array) (rtoken (eql +end+)) set-method next-setter)
  (declare (ignore flavor))
  (values nil nil
	  (ecase set-method
	    ((nil) nil)
	    ((:update :add)
	     (chained-setter-lambda (x) (next-setter obj)
	       (unless (array-try-push obj x)
		 ;; Automatically extends it.
		 (let ((old-length (length obj)))
		   (setf obj (extend-array obj (1+ old-length) old-length)))
		 (vector-push x obj))))
	    ((:delete :remove)
	     (thunk-lambda
	       (bad-deleter-error obj rtoken))))))

(defmethod traverse-by-reference-token (flavor (obj array) (rtoken integer) set-method next-setter)
  (declare (ignore flavor))
  (if (not (array-in-bounds-p obj rtoken))
      (values nil nil
	      (ecase set-method
		((nil) nil)
		((:update :add)
		 (chained-setter-lambda (x) (next-setter obj)
		   ;; Automatically extends it.
		   (setf obj (extend-array obj (1+ rtoken) t)
			 (aref obj rtoken) x)))
		((:delete :remove)
		 (thunk-lambda
		   (bad-deleter-error obj rtoken)))))
      (values (aref obj rtoken) rtoken
	      (ecase set-method
		((nil) nil)
		((:update :add)
		 (chained-setter-lambda (x) (next-setter obj)
		   (setf (aref obj rtoken) x)))
		((:delete :remove)
		 (chained-setter-lambda () (next-setter obj)
		   ;; Fills with NIL. (There is no way to 'remove nil')
		   (setf (aref obj rtoken) nil)))))))

(defmethod traverse-by-reference-token (flavor (obj array) (rtoken string) set-method next-setter)
  (let ((index (read-reference-token-as-index rtoken)))
    (traverse-by-reference-token flavor obj index set-method next-setter)))

;;; Entry Point

(defun traverse-by-json-pointer (obj flavor pointer set-method)
  "Traverses OBJ with a parsed json-pointer (POINTER), and returns three values:
the referred object, existence (boolean), and a closure can be used as a setter.

SET-METHOD determines how to _set_ into OBJ by the returned setter:
- `nil' :: No setters made. (Do not set to OBJ.)
- `:update' :: Destructively updates into OBJ.
- `:delete' :: Destructively deletes from OBJ.
- `:add' :: If changing a list, makes a new list containing the set'ed value. (non-list objs are still modified).
- `:remove' :: If deleting form a list, makes a new list not containing the removed value. (non-list objs are still modified).

FLAVOR is a keyword specifies the JSON object flavors.
See `*cl-json-pointer-supported-json-flavors*'
"
  (let ((value obj)
	(exists? t)
	(setter
	 (if set-method
	     (lambda (&optional x) (setf obj x)))))
    (loop for (rtoken . next) on pointer
       as this-set-method = (ecase set-method
			      ((:add :update nil) set-method)
			      ;; Makes a deleter only at last.
			      (:remove (if next :add :remove))
			      (:delete (if next :update :delete)))
       do (setf (values value exists? setter)
		(traverse-by-reference-token flavor value rtoken this-set-method setter))
       while next)
    (values value exists? setter)))
