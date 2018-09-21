(in-package :cl-json-pointer)

(defconstant +delete-request+
  '+delete-request+
  "A special value indicates deletion, used by setters.")

;;; Switches

(defparameter *traverse-treat-string-as-atom* t
  ;; I don't want to treat string as an array.
  "If this is T, cl-json-pointer trests string as atom.")

(defparameter *traverse-set-to-list-destructive* t
  "Determines set to list destructively or not.")

(defparameter *traverse-consider-plist* nil
  ;; I think there is no way to define good `plist-like-p', because plist
  ;; does not restricted. Whether its keys are compared by `eq'
  ;; (http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm),
  ;; I think I should not assume the keys are always a symbol.
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
     (declare (ignorable ,@vars))
     (funcall ,next-function
	      (progn ,@body ,target))))

;;; Reference Token

(defun read-reference-token-as-index (reference-token &optional (errorp t))
  (cond ((eq reference-token +last-nonexistent-element+)
	 +last-nonexistent-element+)
	((and (> (length reference-token) 1)
	      (char= (char reference-token 0) #\0))
	 (when errorp
	   (error 'json-pointer-bad-index-error
		  :format-control "reference token (~A) should not start with 0 as index"
		  :format-arguments (list reference-token)))
	 nil)
	(t
	 (handler-case (parse-integer reference-token)
	   (error ()
	     (when errorp
	       (error 'json-pointer-bad-index-error
		      :format-control "reference token (~A) cannot be read as index"
		      :format-arguments (list reference-token)))
	     nil)))))


;;; Main traversal.

(defgeneric traverse-by-reference-token (obj rtoken set-method next-setter)
  (:documentation "Traverses an object with a reference token, and
  returns three values: a referred object, existence (boolean), and a
  closure can be used as a setter."))

(defun error-on-traversing-atom (obj rtoken)
  ;; FIXME: I think this should be error, but `silent' option is required..
  (values nil nil
	  (thunk-lambda
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

(defun bad-deleter-error (obj rtoken)
  (error 'json-pointer-not-found-error
	 :format-control "object ~S does not have a point to delete, referenced by ~A"
	 :format-arguments (list obj rtoken)))

(defun traverse-alist-by-reference-token (alist rtoken set-method next-setter)
  ;; accepts `nil' as alist.
  (if-let ((entry (assoc rtoken alist :test #'compare-string-by-readtable-case)))
    (values (cdr entry) entry
	    (ecase set-method
	      ((nil) nil)
	      (:update
	       (chained-setter-lambda (x) (alist next-setter)
		 (if *traverse-set-to-list-destructive*
		     (setf (cdr entry) x)
		     (push (cons rtoken x) alist))))
	      (:delete
	       (chained-setter-lambda () (alist next-setter)
		 (if *traverse-set-to-list-destructive*
		     (deletef alist entry)
		     ;; FIXME: add 'delete-all' method?, or shadows it by `nil'?
		     (removef alist entry))))))
    (values nil nil
	    (ecase set-method
	      ((nil) nil)
	      (:update
	       (chained-setter-lambda (x) (alist next-setter)
		 (push (cons rtoken x) alist)))
	      (:delete
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
			 ;; FIXME: add 'delete-all' method?
			 (if *traverse-set-to-list-destructive*
			     (setf (cadr plist-head) x)
			     (setf plist (list* rtoken x plist)))))
		      (:delete
		       (chained-setter-lambda () (plist next-setter)
			 ;; FIXME: add 'delete-all' method?
			 (if *traverse-set-to-list-destructive*
			     (setf plist (delete-cons plist plist-head 2))
			     (setf plist (remove-cons plist plist-head 2)))))))
     finally
       (return (values nil nil
		       (ecase set-method
			 ((nil) nil)
			 (:update
			  (chained-setter-lambda (x) (plist next-setter)
			    (setf plist (list* rtoken x plist))))
			 (:delete
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
		     (if *traverse-set-to-list-destructive*
			 (nconcf list (list x))
			 (appendf list (list x)))))
		  (:delete
		   (thunk-lambda
		     (bad-deleter-error list rtoken)))))
	(if-let ((this-cons (nthcdr index list)))
	  (values (car this-cons) this-cons
		  (ecase set-method
		    ((nil) nil)
		    (:update
		     (chained-setter-lambda (x) (list next-setter)
		       (if *traverse-set-to-list-destructive*
			   (setf (car this-cons) x)
			   (setf list (make-replaced-list-on-cons list this-cons x)))))
		    (:delete
		     (chained-setter-lambda (x) (list next-setter)
		       (if *traverse-set-to-list-destructive*
			   (setf list (delete-cons list this-cons)) 
			   (setf list (remove-cons list this-cons)))))))
	  (values nil nil
		  (if set-method
		      (thunk-lambda
			(error 'json-pointer-not-found-error
			       :format-control "Index ~A is out-of-index from ~A"
			       :format-arguments (list index list)))))))))

(defmethod traverse-by-reference-token ((obj list) rtoken set-method next-setter)
  (if (read-reference-token-as-index rtoken nil)
      ;; rtoken is ambiguous with index.
      (cond ((alist-like-p obj)
	     (traverse-alist-by-reference-token obj rtoken set-method next-setter))
	    ((and *traverse-consider-plist*
		  (plist-like-p obj))
	     (traverse-plist-by-reference-token obj rtoken set-method next-setter))
	    (t
	     (traverse-ordinal-list-by-reference-token obj rtoken set-method next-setter)))
      ;; I assume `rtoken' is not index, so considers alist (or plist).
      (if (and *traverse-consider-plist*
	       (plist-like-p obj))
	  (traverse-plist-by-reference-token obj rtoken set-method next-setter)
	  (traverse-alist-by-reference-token obj rtoken set-method next-setter))))

(defmethod traverse-by-reference-token ((obj null) rtoken set-method next-setter)
  ;; empty. this is problematic for setting.
  (let* ((index (read-reference-token-as-index rtoken nil))
	 (setter-method
	  (cond ((eq index +last-nonexistent-element+)
		 *traverse-nil-set-to-last-method*)
		((integerp index)
		 *traverse-nil-set-to-index-method*)
		(t
		 *traverse-nil-set-to-name-method*)))
	 (setter
	  (if set-method
	      (flet ((pick-setter (func)
		       (nth-value 2 (funcall func obj rtoken set-method next-setter))))
		(ecase setter-method
		  (:list
		   (pick-setter #'traverse-ordinal-list-by-reference-token))
		  (:alist
		   (pick-setter #'traverse-alist-by-reference-token))
		  (:plist
		   (pick-setter #'traverse-plist-by-reference-token))
		  (:array
		   (make-array-tail-adder #() next-setter
					  :set-to-last-method :create))
		  (:error
		   (thunk-lambda
		     (error 'json-pointer-access-error
			    :format-control "Set to nil by index is not supported"))))))))
    (values nil nil setter)))

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
		(:update
		 (chained-setter-lambda (x) (obj next-setter)
		   (setf (slot-value-using-class class obj slot) x)))
		(:delete
		 (chained-setter-lambda (x) (obj next-setter)
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

(defmethod traverse-by-reference-token ((obj hash-table) rtoken set-method next-setter)
  ;; TODO: use `compare-string-by-readtable-case' (depending on json lib..)
  (multiple-value-bind (value exists?)
      (gethash rtoken obj)
    (values value exists?
	    (ecase set-method
	      ((nil) nil)
	      (:update
	       (chained-setter-lambda (x) (obj next-setter)
		 (setf (gethash rtoken obj) x)))
	      (:delete
	       (chained-setter-lambda (x) (obj next-setter)
		 (remhash rtoken obj)))))))

(defun make-array-tail-adder (array next-setter
			      &key (set-to-last-method *traverse-non-adjustable-array-set-to-last-method*))
  (lambda (x)
    (check-type array (array * (*)))
    (when (eq x +delete-request+)
      (bad-deleter-error array +last-nonexistent-element+))
    (let ((adjustable? (adjustable-array-p array))
	  (has-fill-pointer? (array-has-fill-pointer-p array)))
      (cond ((and adjustable?
		  has-fill-pointer?)
	     (vector-push-extend x array))
	    ((and has-fill-pointer?
		  (vector-push x array))) ; uses `vector-push' result as condition.
	    (t
	     (ecase set-to-last-method
	       (:error
		(error 'json-pointer-access-error
		       :format-control "tried to add to the tail of non-adjutable non-fill-pointer array"))
	       (:create
		(let* ((original-length (length array))
		       (new-array (make-array (1+ original-length)
					      :adjustable t
					      :fill-pointer original-length)))
		  (replace new-array array)
		  (setf array new-array))
		(vector-push x array))))))
    (funcall next-setter array)))

(defmethod traverse-by-reference-token ((obj array) rtoken set-method next-setter)
  (let ((index (read-reference-token-as-index rtoken)))
    (cond ((eq index +last-nonexistent-element+)
	   (values nil nil
		   (if set-method
		       (make-array-tail-adder obj next-setter))))
	  ((not (array-in-bounds-p obj index))
	   (values nil nil
		   (if set-method
		       (thunk-lambda
			 (error 'json-pointer-not-found-error
				:format-control "Index ~A (pointer ~A) is out-of-index from ~A"
				:format-arguments (list index rtoken obj))))))
	  (t
	   (values (aref obj index) index
		   (ecase set-method
		     ((nil) nil)
		     (:update
		      (chained-setter-lambda (x) (obj next-setter)
			(setf (aref obj index) x)))
		     (:delete
		      (chained-setter-lambda (x) (obj next-setter)
			(ecase *traverse-array-delete-method*
			  ((nil) (setf (aref obj index) x))
			  (:error
			   (error 'json-pointer-access-error
				  :format-control "Delete from array is error (array ~A, index ~A)"
				  :format-arguments (list obj index))))))))))))

(defun traverse-by-json-pointer (obj parsed-pointer set-method)
  "Traverses an object with a parsed json-pointer, and returns three values:
the referred object, existence (boolean), and a closure can be used as a setter.

`set-method' determines how to *set* into `obj' by the returned setter:
- `nil' :: Do not set to `obj'.
- `:update' :: Destructively updates into `obj'.
- `:delete' :: Destructively deletes from `obj'.
- `:add' :: Returns new `obj' contains the set'ed value. (not destructive)
- `:remove' :: Returns new `obj' does not contain the removed value. (not destructive)
"
  (assert (not (member set-method '(:remove :add)))
	  () "Sorry, not implemented set-method ~A" set-method)
  (let ((value obj)
	(exists? t)
	(setter
	 (if set-method
	     (lambda (x) (setf obj x)))))
    (loop for (rtoken . next) on parsed-pointer
       as this-set-method = (ecase set-method
			      ((:add :update nil) set-method)
			      ;; Makes a deleter only at last.
			      (:remove (if next :add :remove))
			      (:delete (if next :update :delete)))
       do (setf (values value exists? setter)
		(traverse-by-reference-token value rtoken this-set-method setter))
       while next)
    (values value exists? setter)))
