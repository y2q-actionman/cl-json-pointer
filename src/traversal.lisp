(in-package :cl-json-pointer)

;;; Switches

(defparameter *traverse-treat-string-as-atom* t
  ;; I don't want to treat string as an array.
  "If this is T, cl-json-pointer trests string as atom.")

(defparameter *traverse-consider-plist* nil
  "If this is T, cl-json-pointer considers plists at traversaling.")

(defparameter *traverse-nil-set-to-last-method* :list
  "Determines how to set to the last (by '-') of NIL.
- `:list' :: pushes <value> as an ordinal list.
- `:alist' :: pushes (reference-token . <value>) as an alist.
- `:plist' :: appends (reference-token <value>) as an plist.
- `:array' :: makes a new array contains <value>.
")

(defparameter *traverse-nil-set-to-index-method* :error
  "Determines how to set to NIL by an index.
- `:error' :: throws an error.
- `:alist' :: pushes (reference-token . <value>) as an alist.
- `:plist' :: appends (reference-token <value>) as an plist.
")

(defparameter *traverse-nil-set-to-name-method* :alist
  "Determines how to set to NIL by a name.
- `:alist' :: pushes (reference-token . <value>) as an alist.
- `:plist' :: appends (reference-token <value>) as an plist.
")

(defparameter *traverse-non-adjustable-array-set-to-last-method* :create
  "Determines how to set to the last (by '-') of non-adjutable arrays.
- `:error' :: throws an error.
- `:create' :: makes a new adjustable array contains <value>.
")

(defparameter *traverse-array-delete-method* nil
  "Determines how to delete an value of arrays.
- `nil' :: fills nil.
- `:error' :: throws an error.
")

;;; Tools

(defmacro chained-setter-lambda ((&rest vars) (target next-function) &body body)
  `(lambda (,@vars)
     (funcall ,next-function
	      (progn ,@body ,target))))

;;; Reference Token

(defun read-reference-token-as-index (rtoken &optional (errorp t))
  (etypecase rtoken
    (integer rtoken)
    (symbol (assert (eq rtoken +last-nonexistent-element+)
		    () 'json-pointer-bad-reference-token-error
		    :format-control "reference token (~A) is not a known symbol"
		    :format-arguments (list rtoken))
	    rtoken)
    (string
     (cond ((and (> (length rtoken) 1)
		 (char= (char rtoken 0) #\0)) ; RFC6901 does not allow '0' at the beginning.
	    (if errorp
		(error 'json-pointer-bad-reference-token-error
		       :format-control "reference token (~A) must not start with '0' when used as an index"
		       :format-arguments (list rtoken))))
	   (t
	    (handler-case (parse-integer rtoken)
	      (error ()
		(if errorp
		    (error 'json-pointer-bad-reference-token-error
			   :format-control "reference token (~A) cannot be read as index"
			   :format-arguments (list rtoken))))))))))

;;; Main traversal.

(defgeneric traverse-by-reference-token (obj rtoken set-method next-setter)
  (:documentation "Traverses an object with a reference token, and
  returns three values: a referred object, existence (boolean), and a
  closure can be used as a setter."))

(defun bad-deleter-error (obj rtoken)
  (error 'json-pointer-access-error
	 :format-control "Object ~S's point ~A is not a place to delete"
	 :format-arguments (list obj rtoken)))

(defun out-of-index-error (obj rtoken)
  (error 'json-pointer-access-error
	 :format-control "Object ~S's point ~A is out-of-index"
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

(defun traverse-alist-by-reference-token (alist rtoken set-method next-setter)
  ;; accepts `nil' as alist.
  (if-let ((entry (assoc rtoken alist :test #'compare-string-by-readtable-case)))
    (values (cdr entry) entry
	    (ecase set-method
	      ((nil) nil)
	      (:update
	       (chained-setter-lambda (x) (alist next-setter)
		 (setf (cdr entry) x)))
	      (:add
	       (chained-setter-lambda (x) (alist next-setter)
		 (push (cons rtoken x) alist)))
	      (:delete
	       (chained-setter-lambda () (alist next-setter)
		 (deletef alist entry)))
	      (:remove
	       (chained-setter-lambda () (alist next-setter)
		 (removef alist entry)))))
    (values nil nil
	    (ecase set-method
	      ((nil) nil)
	      ((:add :update)
	       (chained-setter-lambda (x) (alist next-setter)
		 (push (cons rtoken x) alist)))
	      ((:delete :remove)
	       (thunk-lambda
		 (bad-deleter-error alist rtoken)))))))

(defun traverse-plist-by-reference-token (plist rtoken set-method next-setter)
  ;; accepts `nil' as plist.
  (loop for plist-head on plist by #'cddr
     as (k v) = plist-head
     when (compare-string-by-readtable-case k rtoken) ; plist often uses `eq', but I use this.
     return (values v plist-head
		    (ecase set-method
		      ((nil) nil)
		      (:update
		       (chained-setter-lambda (x) (plist next-setter)
			 (setf (cadr plist-head) x)))
		      (:add
		       (chained-setter-lambda (x) (plist next-setter)
			 (setf plist (list* rtoken x plist))))
		      (:delete
		       (chained-setter-lambda () (plist next-setter)
			 (setf plist (delete-cons plist plist-head 2))))
		      (:remove
		       (chained-setter-lambda () (plist next-setter)
			 (setf plist (remove-cons plist plist-head 2))))))
     finally
       (return (values nil nil
		       (ecase set-method
			 ((nil) nil)
			 ((:update :add)
			  (chained-setter-lambda (x) (plist next-setter)
			    (setf plist (list* rtoken x plist))))
			 ((:delete :remove)
			  (thunk-lambda
			    (bad-deleter-error plist rtoken))))))))
		       
(defun traverse-ordinal-list-by-reference-token (list rtoken set-method next-setter)
  (let ((index (read-reference-token-as-index rtoken)))
    (if (eq index +last-nonexistent-element+)
	(values nil nil
		(ecase set-method
		  ((nil) nil)
		  (:update
		   (chained-setter-lambda (x) (list next-setter)
		     (nconcf list (list x))))
		  (:add
		   (chained-setter-lambda (x) (list next-setter)
		     (appendf list (list x))))
		  ((:delete :remove)
		   (thunk-lambda
		     (bad-deleter-error list rtoken)))))
	(if-let ((this-cons (nthcdr index list)))
	  (values (car this-cons) this-cons
		  (ecase set-method
		    ((nil) nil)
		    (:update
		     (chained-setter-lambda (x) (list next-setter)
		       (setf (car this-cons) x)))
		    (:add
		     (chained-setter-lambda (x) (list next-setter)
		       (setf list (clone-and-replace-on-cons list this-cons x))))
		    (:delete
		     (chained-setter-lambda () (list next-setter)
		       (setf list (delete-cons list this-cons))))
		    (:remove
		     (chained-setter-lambda () (list next-setter)
		       (setf list (remove-cons list this-cons))))))
	  (values nil nil
		  (if set-method
		      (thunk-lambda
			(out-of-index-error list index))))))))

(defmethod traverse-by-reference-token ((obj list) rtoken set-method next-setter)
  ;; `rtoken' may be ambiguous with index.
  ;; I think how to treat `rtoken' depends on the list structure of `obj'.
  (cond ((alist-like-p obj)
	 (traverse-alist-by-reference-token obj rtoken set-method next-setter))
	((and *traverse-consider-plist*
	      (plist-like-p obj))
	 (traverse-plist-by-reference-token obj rtoken set-method next-setter))
	(t
	 (traverse-ordinal-list-by-reference-token obj rtoken set-method next-setter))))

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
		     (cond ((eq index +last-nonexistent-element+)
			    *traverse-nil-set-to-last-method*)
			   ((integerp index)
			    *traverse-nil-set-to-index-method*)
			   (t
			    *traverse-nil-set-to-name-method*))))
	       (flet ((pick-setter (func)
			(nth-value 2 (funcall func obj rtoken set-method next-setter))))
		 (ecase nil-method
		   (:list
		    (pick-setter #'traverse-ordinal-list-by-reference-token))
		   (:alist
		    (pick-setter #'traverse-alist-by-reference-token))
		   (:plist
		    (pick-setter #'traverse-plist-by-reference-token))
		   (:array
		    (chained-setter-lambda (x) (obj next-setter)
		      (setf obj (add-to-array-tail obj x))))
		   (:error
		    (thunk-lambda
		      (error 'json-pointer-access-error
			     :format-control "Set to nil by index is not supported"))))))))))

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
		 (chained-setter-lambda (x) (obj next-setter)
		   (setf (slot-value-using-class class obj slot) x)))
		((:remove :delete)
		 (chained-setter-lambda () (obj next-setter)
		   (slot-makunbound-using-class class obj slot))))))
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
	       (chained-setter-lambda (x) (obj next-setter)
		 (setf (gethash rtoken obj) x)))
	      ((:delete :remove)
	       (chained-setter-lambda () (obj next-setter)
		 (remhash rtoken obj)))))))

;;; Array

(defun array-try-push (array x)
  (let ((adjustable? (adjustable-array-p array))
	(has-fill-pointer? (array-has-fill-pointer-p array)))
    (if has-fill-pointer?
	(if adjustable?
	    (vector-push-extend x array)
	    (vector-push x array)) ; uses `vector-push' result as condition.
	nil)))

(defun add-to-array-tail (array x)
  ;; `array' may be NIL, because this may be called at traversing NIL.
  (let ((pushed? (if (arrayp array)
		     (array-try-push array x)
		     nil)))
    (unless pushed?
      (ecase *traverse-non-adjustable-array-set-to-last-method*
	(:error
	 (error 'json-pointer-access-error
		:format-control "tried to add to the tail of non-adjutable non-fill-pointer array"))
	(:create
	 (let* ((o-length (length array))
		(new-array (make-array (1+ o-length) :adjustable t :fill-pointer o-length)))
	   (replace new-array array)
	   (setf array new-array))
	 (vector-push x array)))))
  array)

(defmethod traverse-by-reference-token ((obj array) (rtoken (eql +last-nonexistent-element+))
					set-method next-setter)
  (values nil nil
	  (ecase set-method
	    ((nil) nil)
	    ((:update :add)
	     (chained-setter-lambda (x) (obj next-setter)
	       (setf obj (add-to-array-tail obj x))))
	    ((:delete :remove)
	     (thunk-lambda
	       (bad-deleter-error obj rtoken))))))

(defmethod traverse-by-reference-token ((obj array) (rtoken integer) set-method next-setter)
  (if (not (array-in-bounds-p obj rtoken))
      (values nil nil
	      (if set-method
		  (thunk-lambda
		    (out-of-index-error obj rtoken))))
      (values (aref obj rtoken) rtoken
	      (ecase set-method
		((nil) nil)
		((:update :add)
		 (chained-setter-lambda (x) (obj next-setter)
		   (setf (aref obj rtoken) x)))
		((:delete :remove)
		 (chained-setter-lambda () (obj next-setter)
		   (ecase *traverse-array-delete-method*
		     ((nil) (setf (aref obj rtoken) nil))
		     (:error
		      (error 'json-pointer-access-error
			     :format-control "Delete from array is error (array ~A, index ~A)"
			     :format-arguments (list obj rtoken))))))))))

(defmethod traverse-by-reference-token ((obj array) (rtoken string) set-method next-setter)
  (let ((index (read-reference-token-as-index rtoken)))
    (traverse-by-reference-token obj index set-method next-setter)))

;;; Entry Point

(defun traverse-by-json-pointer (obj pointer set-method)
  "Traverses an object with a parsed json-pointer, and returns three values:
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
	     (lambda (x) (setf obj x)))))
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
