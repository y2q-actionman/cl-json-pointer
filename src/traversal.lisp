(in-package :cl-json-pointer)

;;; Switches

(defvar *traverse-treat-string-as-atom* t
  ;; I don't want to treat string as an array.
  "If this is T, cl-json-pointer trests string as atom.")

(defvar *traverse-nil-set-to-last-method* :list
  "Determines how to set to the last (by '-') of NIL.
- `:list' :: pushes <value> as an ordinal list.
- `:alist' :: pushes (reference-token . <value>) as an alist.
- `:plist' :: appends (reference-token <value>) as an plist.
- `:array' :: makes a new array contains <value>.
")

(defvar *traverse-nil-set-to-index-method* :list
  "Determines how to set to NIL by an index.
- `:list' :: makes a new list and set <value> into nth point.
- `:alist' :: pushes (reference-token . <value>) as an alist.
- `:plist' :: appends (reference-token <value>) as an plist.
- `:error' :: throws an error.
")

(defvar *traverse-nil-set-to-name-method* :alist
  "Determines how to set to NIL by a name.
- `:alist' :: pushes (reference-token . <value>) as an alist.
- `:plist' :: appends (reference-token <value>) as an plist.
- `:jsown' :: makes a jsown-style object.
")

(defvar *traverse-object-like-kinds* '(:alist))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :com.gigamonkeys.json)
    (pushnew :plist *traverse-object-like-kinds*)))

;;; Tools

(defmacro chained-setter-lambda
    ((&rest vars) (next-function &optional (next-arg nil na-supplied-p)) &body body)
  `(lambda (,@vars)
     (funcall ,next-function
	      (progn ,@body
		     ,@(if na-supplied-p `(,next-arg) ())))))

;;; Reference Token

(defun read-reference-token-as-index (rtoken &optional (errorp t))
  (etypecase rtoken
    (integer rtoken)
    (symbol (assert (eq rtoken +end+)
		    () 'json-pointer-bad-reference-token-error
		    :reference-token rtoken
		    :format-control "reference token (~A) is not a known symbol")
	    rtoken)
    (string
     (flet ((error-if-required (&rest error-args)
	      (let ((e (apply #'make-condition error-args)))
		(if errorp (error e) e))))
       (cond ((and (> (length rtoken) 1)
		   (char= (char rtoken 0) #\0)) ; RFC6901 does not allow '0' at the beginning.
	      (values nil
		      (error-if-required 'json-pointer-bad-reference-token-0-used-error
					 :reference-token rtoken)))
	     (t
	      (handler-case (parse-integer rtoken)
		(error ()
		  (values nil
			  (error-if-required 'json-pointer-bad-reference-token-not-numeric-error
					     :reference-token rtoken))))))))))

;;; Main traversal.

(defgeneric traverse-by-reference-token (obj rtoken set-method next-setter)
  (:documentation "Traverses an object with a reference token, and
  returns three values: a referred object, existence (boolean), and a
  closure can be used as a setter."))

(defun bad-deleter-error (obj rtoken)
  (error 'json-pointer-access-error
	 :format-control "Object ~S's point ~A is not a place to delete"
	 :format-arguments (list obj rtoken)))

;;; Atoms

(defun error-on-traversing-atom (obj rtoken)
  ;; FIXME: I think this should be error, but `silent' option is required..
  (values nil nil
	  (thunk-lambda
	    ;; this is the only point using `json-pointer-not-found-error'
	    (error 'json-pointer-not-found-error
		   :format-control "obj ~S is not an array or an object (pointer is ~A)"
		   :format-arguments (list obj rtoken)))))

(defmethod traverse-by-reference-token (obj rtoken set-method next-setter)
  (declare (ignore set-method next-setter))
  ;; bottom case -- refers an unsupported type object.
  (error-on-traversing-atom obj rtoken))

(defmethod traverse-by-reference-token ((obj string) rtoken set-method next-setter)
  (declare (ignore set-method next-setter))
  (if *traverse-treat-string-as-atom*
      (error-on-traversing-atom obj rtoken)
      (call-next-method)))

;;; List

(defgeneric traverse-list-by-reference-token (list-kind list rtoken set-method next-setter))

(defmethod traverse-list-by-reference-token ((kind (eql :alist)) alist rtoken
					     set-method next-setter)
  ;; accepts `nil' as alist.
  (if-let ((entry (assoc rtoken alist :test #'compare-string-by-readtable-case)))
    (values (cdr entry) entry
	    (ecase set-method
	      ((nil) nil)
	      (:update
	       (chained-setter-lambda (x) (next-setter alist)
		 (setf (cdr entry) x)))
	      (:add
	       (chained-setter-lambda (x) (next-setter)
		 (acons rtoken x alist)))
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
		 (acons rtoken x alist)))
	      ((:delete :remove)
	       (thunk-lambda
		 (bad-deleter-error alist rtoken)))))))

(defmethod traverse-list-by-reference-token ((kind (eql :plist)) plist rtoken
					     set-method next-setter)
  ;; accepts `nil' as plist.
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
			 (list* rtoken x plist)))
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
			    (list* rtoken x plist)))
			 ((:delete :remove)
			  (thunk-lambda
			    (bad-deleter-error plist rtoken))))))))
		       
(defmethod traverse-list-by-reference-token ((kind (eql :list)) list
					     (rtoken (eql +end+)) set-method next-setter)
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
	
(defmethod traverse-list-by-reference-token ((kind (eql :list)) list
					     (index integer) set-method next-setter)
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

(defmethod traverse-list-by-reference-token ((kind (eql :list)) list
					     (rtoken string) set-method next-setter)
  (traverse-list-by-reference-token kind list
				    (read-reference-token-as-index rtoken)
				    set-method next-setter))

(defmethod traverse-by-reference-token ((obj list) (rtoken (eql +end+)) set-method next-setter)
  (traverse-list-by-reference-token :list obj rtoken set-method next-setter))

(defmethod traverse-by-reference-token ((obj list) (rtoken string) set-method next-setter)
  (let* ((kinds *traverse-object-like-kinds*)
	 (try-result-alist nil))
    ;; `rtoken' may be ambiguous with an index or a name of object fields.
    ;; 
    ;; 1. Try to use it as an existed field name.
    ;; FIXME: This loop is too heavy! (but I think it is required..)
    (loop for kind in kinds
       as ret =
	 (handler-case
	     (multiple-value-list
	      (traverse-list-by-reference-token kind obj rtoken set-method next-setter))
	   (error () nil))
       if (second ret)			; exists?
       do (return-from traverse-by-reference-token
	    (values-list ret))
       else
       do (push (cons kind ret) try-result-alist)
       finally (nreversef try-result-alist))
    ;; `rtoken' is not a name of object fields.
    ;;
    ;; 2. If it can be read as an index, I treat `obj' as an ordinal list.
    (multiple-value-bind (index bad-index-condition)
	(read-reference-token-as-index rtoken nil)
      (when index			; yes, an ordinal list!
	(return-from traverse-by-reference-token
	  (traverse-list-by-reference-token :list obj rtoken set-method next-setter)))
      (when (typep bad-index-condition 'json-pointer-bad-reference-token-0-used-error)
	(error bad-index-condition)))
    ;; 3. `rtoken' assumed as a field name, but not found.
    ;; 3-1. use the specified default.
    (when-let* ((default (assoc *traverse-nil-set-to-name-method* try-result-alist))
		(default-setter (third (cdr default))))
      (return-from traverse-by-reference-token
	(values nil nil default-setter)))
    ;; 3-2. use a found one.
    (loop for (nil nil nil setter) in try-result-alist
       when setter
       return (return-from traverse-by-reference-token
		(values nil nil setter)))
    ;; 3-3. no way...
    (values nil nil
	    (if set-method
		(thunk-lambda
		  (error 'json-pointer-not-found-error
			 :format-control "There is no way to set to ~A (rtoken ~A)"
			 :format-arguments (list obj rtoken)))))))

(defmethod traverse-by-reference-token ((obj null) rtoken set-method next-setter)
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
		 ((:list :alist :plist :jsown)
		  (nth-value 2 (traverse-list-by-reference-token
				nil-method obj rtoken set-method next-setter)))
		 (:array
		  (chained-setter-lambda (x) (next-setter)
		    (make-array 1 :adjustable t :initial-element x :fill-pointer t)))
		 (:error
		  (thunk-lambda
		    (error 'json-pointer-access-error
			   :format-control "Set to nil by index is not supported")))))))))

;;; Objects

(defun traverse-by-reference-token-using-class (obj rtoken set-method next-setter class)
  (if-let ((slot (find rtoken (class-slots class)
		       :key #'slot-definition-name
		       :test #'compare-string-by-readtable-case)))
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
	    (if set-method
		(thunk-lambda
		  (error 'json-pointer-access-error
			 :format-control "object ~A does not have '~A' slot"
			 :format-arguments (list obj rtoken)))))))

(defmethod traverse-by-reference-token ((obj standard-object) rtoken set-method next-setter)
  ;; cl-json:fluid-object can be treated here.
  (traverse-by-reference-token-using-class obj rtoken set-method next-setter (class-of obj)))

(defmethod traverse-by-reference-token ((obj structure-object) rtoken set-method next-setter)
  (traverse-by-reference-token-using-class obj rtoken set-method next-setter (class-of obj)))

;;; Hash table

(defmethod traverse-by-reference-token ((obj hash-table) rtoken set-method next-setter)
  ;; TODO: use `compare-string-by-readtable-case' (depending on json lib..)
  (multiple-value-bind (value exists?)
      (gethash rtoken obj)
    (values value exists?
	    (ecase set-method
	      ((nil) nil)
	      ((:add :update) 
	       (chained-setter-lambda (x) (next-setter obj)
		 (setf (gethash rtoken obj) x)))
	      ((:delete :remove)
	       (if exists?
		   (chained-setter-lambda () (next-setter obj)
		     (remhash rtoken obj))
		   (thunk-lambda
		     (error 'json-pointer-access-error
			    :format-control "Hash-table ~A does not have ~A key"
			    :format-arguments (list obj rtoken)))))))))

;;; Array

(defmethod traverse-by-reference-token ((obj array) (rtoken (eql +end+)) set-method next-setter)
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

(defmethod traverse-by-reference-token ((obj array) (rtoken integer) set-method next-setter)
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

(defmethod traverse-by-reference-token ((obj array) (rtoken string) set-method next-setter)
  (let ((index (read-reference-token-as-index rtoken)))
    (traverse-by-reference-token obj index set-method next-setter)))

;;; Entry Point

(defun traverse-by-json-pointer (obj pointer set-method)
  "Traverses `obj' with a parsed json-pointer (`pointer'), and returns three values:
the referred object, existence (boolean), and a closure can be used as a setter.

`set-method' determines how to *set* into `obj' by the returned setter:
- `nil' :: No setters made. (Do not set to `obj'.)
- `:update' :: Destructively updates into `obj'.
- `:delete' :: Destructively deletes from `obj'.
- `:add' :: If changing a list, makes a new list containing the set'ed value. (non-list objs are still modified).
- `:remove' :: If deleting form a list, makes a new list not containing the removed value. (non-list objs are still modified).
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
		(traverse-by-reference-token value rtoken this-set-method setter))
       while next)
    (values value exists? setter)))
