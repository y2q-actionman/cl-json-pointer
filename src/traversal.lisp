(in-package :cl-json-pointer)

(defmacro aconsf-internal (ref key value)
  `(acons ,key ,value ,ref))

(define-modify-macro aconsf (key value)
  aconsf-internal)

(defmacro list*-f-internal (ref &rest values)
  `(list* ,@values ,ref))

(define-modify-macro list*-f (&rest values)
  list*-f-internal)

(defmacro setf-lambda (access-form)
  "Used for a simple setter."
  (with-gensyms (x)
    `(lambda (,x) (setf ,access-form ,x))))

(defmacro thunk-lambda (&body form)
  "Used for delayung errors."
  (with-gensyms (_)
    `(lambda (&rest ,_)
       (declare (ignore ,_))
       ,@form)))


(defun read-reference-token-as-index (reference-token)
  (cond ((eq reference-token +last-nonexistent-element+)
	 +last-nonexistent-element+)
	((and (> (length reference-token) 1)
	      (char= (char reference-token 0) #\0))
	 ;; FIXME: we must distinguish this from below.
	 (error 'json-pointer-not-found-error
		:format-control "reference token (~A) should not start with 0 as index"
		:format-arguments (list reference-token)))
	(t
	 (handler-case (parse-integer reference-token)
	   (error ()
	     (error 'json-pointer-not-found-error
		    :format-control "reference token (~A) cannot be read as index"
		    :format-arguments (list reference-token)))))))

;;; TODO: merges `make-setter?' and `parental-setter'

(defgeneric traverse-by-reference-token (obj rtoken parental-setter)
  (:documentation "Traverses an object with a reference token, and
  returns three values: a referred object, existence (boolean), and a
  closure can be used as a setter."))

(defmethod traverse-by-reference-token (obj rtoken parental-setter)
  ;; bottom case 1 -- refers an unsupported type object.
  (declare (ignore parental-setter))
  (error 'json-pointer-not-found-error
	 :format-control "obj ~A is not an array or an object (pointer is ~A)"
	 :format-arguments (list obj rtoken)))

(defmethod traverse-by-reference-token (obj (rtoken null) parental-setter)
  ;; bottom case 2 -- refers an object with an empty token.
  (values obj obj
	  (if parental-setter
	      (thunk-lambda
		(error 'json-pointer-access-error
		       :format-control "setting with an empty reference token is not supported (object is ~A)"
		       :format-arguments (list obj))))))


(defun compare-string-by-case (a b &optional (case (readtable-case *readtable*)))
  ;; TODO: should I use `ignore-errors' for alist (or plist) ?
  (ecase case
    ((:upcase :downcase) (string-equal a b))
    ((:preserve :invert) (string= a b))))


(defun traverse-alist-by-reference-token (alist rtoken parental-setter)
  ;; accepts `nil' as alist.
  (if-let ((entry (assoc rtoken alist :test #'compare-string-by-case)))
    (values (cdr entry) entry
	    (if parental-setter
		(setf-lambda (cdr entry))))
    (values nil nil
	    (if parental-setter
		(compose parental-setter
			 (lambda (x) (aconsf alist rtoken x)))))))

(defun traverse-plist-by-reference-token (plist rtoken parental-setter)
  ;; accepts `nil' as plist.
  (loop for plist-head on plist by #'cddr
     as (k v) = plist-head
     when (compare-string-by-case k rtoken) ; plist often uses `eq', but we use `string='.
     return (values v plist-head
		    (if parental-setter
			(setf-lambda (cadr plist-head))))
     finally
       (return (values nil nil
		       (if parental-setter
			   (compose parental-setter
				    (lambda (x) (list*-f plist rtoken x))))))))

(defun traverse-ordinal-list-by-reference-token (list rtoken parental-setter)
  (let ((index (read-reference-token-as-index rtoken)))
    (if (eq index +last-nonexistent-element+)
	(values nil nil
		(if parental-setter
		    (compose parental-setter
			     (lambda (x) (nconcf list (list x))))))
	(if-let ((this-cons (nthcdr index list)))
	  (values (car this-cons)	this-cons
		  (if parental-setter
		      (setf-lambda (car this-cons))))
	  (values nil nil
		  (if parental-setter
		      (thunk-lambda
			(error 'json-pointer-not-found-error
			       :format-control "Index ~A is out-of-index from ~A"
			       :format-arguments (list index list)))))))))


(defun alist-like-p (list)
  (every #'consp list))

(defun plist-like-p (list)
  (loop for (k nil) on list by #'cddr
     always (symbolp k)))

(defparameter *traverse-consider-plist* nil
  ;; I think there is no way to define good `plist-like-p', because plist
  ;; does not restricted. Whether its keys are compared by `eq'
  ;; (http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm),
  ;; I think I should not assume the keys are always a symbol.
  "If this is T, cl-json-pointer considers plists at traversaling.")

(defmethod traverse-by-reference-token ((obj list) rtoken parental-setter)
  (if (ignore-errors
	(read-reference-token-as-index rtoken))
    ;; rtoken is ambiguous with index.
    (cond ((alist-like-p obj)
	   (traverse-alist-by-reference-token obj rtoken parental-setter))
	  ((and *traverse-consider-plist*
		(plist-like-p obj))
	   (traverse-plist-by-reference-token obj rtoken parental-setter))
	  (t
	   (traverse-ordinal-list-by-reference-token obj rtoken parental-setter)))
    ;; I assume `rtoken' is not index, so considers alist (or plist).
    (if (and *traverse-consider-plist*
	     (plist-like-p obj))
	(traverse-plist-by-reference-token obj rtoken parental-setter)
	(traverse-alist-by-reference-token obj rtoken parental-setter))))

(defparameter *traverse-nil-set-to-last-method* :list
  "Determines how to set to the last (by '-') of NIL.
- :list :: pushes <value> as an ordinal list.
- :alist :: pushes (reference-token . <value>) as an alist.
- :plist :: appends (reference-token <value>) as an plist.
- :array :: makes a new array contains <value>.
")

(defparameter *traverse-nil-set-to-index-method* :error
  "Determines how to set to NIL by an index.
- :error :: throws an error.
- :alist :: pushes (reference-token . <value>) as an alist.
- :plist :: appends (reference-token <value>) as an plist.
")

(defparameter *traverse-nil-set-to-name-method* :alist
  "Determines how to set to NIL by a name.
- :alist :: pushes (reference-token . <value>) as an alist.
- :plist :: appends (reference-token <value>) as an plist.
")

(defmethod traverse-by-reference-token ((obj null) rtoken parental-setter)
  ;; empty. this is problematic for setting.
  (let* ((index (ignore-errors
		  (read-reference-token-as-index rtoken)))
	 (setter-method
	  (cond ((eq index +last-nonexistent-element+)
		 *traverse-nil-set-to-last-method*)
		((integerp index)
		 *traverse-nil-set-to-index-method*)
		(t
		 *traverse-nil-set-to-name-method*)))
	 (setter
	  (if parental-setter
	      (flet ((pick-setter (func)
		       (nth-value 2 (funcall func obj rtoken parental-setter))))
		(ecase setter-method
		  (:list
		   (pick-setter #'traverse-ordinal-list-by-reference-token))
		  (:alist
		   (pick-setter #'traverse-alist-by-reference-token))
		  (:plist
		   (pick-setter #'traverse-plist-by-reference-token))
		  (:array
		   (make-array-tail-adder #() parental-setter
					  :set-to-last-method :create))
		  (:error
		   (thunk-lambda
		     (error 'json-pointer-access-error
			    :format-control "Set to nil by index is not supported"))))))))
    (values nil nil setter)))

(defmethod traverse-by-reference-token ((obj standard-object) rtoken parental-setter)
  ;; cl-json:fluid-object can be treated here.
  (let* ((class (class-of obj))
	 (slot (find rtoken (class-slots class)
		     :key #'slot-definition-name
		     :test #'compare-string-by-case)))
    (if slot
	(let ((bound? (slot-boundp-using-class class obj slot)))
	  (values (if bound?
		      (slot-value-using-class class obj slot))
		  bound?
		  (if parental-setter
		      (setf-lambda (slot-value-using-class class obj slot)))))
	(values nil nil
		(if parental-setter
		    (thunk-lambda
		      (error 'json-pointer-access-error
			     :format-control "object ~A does not have '~A' slot"
			     :format-arguments (list obj rtoken))))))
    ;; TODO: support structure-object?
    ;; TODO: support condition-object?
    ))

(defmethod traverse-by-reference-token ((obj hash-table) rtoken parental-setter)
  (multiple-value-bind (value exists?)
      (gethash rtoken obj)
    (values value
	    exists?
	    (if parental-setter
		(setf-lambda (gethash rtoken obj))))))

(defparameter *traverse-non-adjustable-array-set-to-last-method* :create
  "Determines how to set to the last (by '-') of non-adjutable arrays.
- :error :: throws an error.
- :create :: makes a new adjustable array contains <value>.
")

(defun make-array-tail-adder (array parental-setter
			      &key (set-to-last-method *traverse-non-adjustable-array-set-to-last-method*))
  (lambda (x)
    (check-type array (array * (*)))
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
		(ensure-function parental-setter)
		(let* ((original-length (length array))
		       (new-array (make-array (1+ original-length)
					      :adjustable t
					      :fill-pointer original-length)))
		  (replace new-array array)
		  (setf array new-array)
		  (funcall parental-setter array))
		(vector-push x array))))))))

(defmethod traverse-by-reference-token ((obj array) rtoken parental-setter)
  (let ((index (read-reference-token-as-index rtoken)))
    (cond ((eq index +last-nonexistent-element+)
	   (values nil nil
		   (if parental-setter
		       (make-array-tail-adder obj parental-setter))))
	  ((not (array-in-bounds-p obj index))
	   (values nil nil
		   (if parental-setter
		       (thunk-lambda
			 (error 'json-pointer-not-found-error
				:format-control "Index ~A (pointer ~A) is out-of-index from ~A"
				:format-arguments (list index rtoken obj))))))
	  (t
	   (values (aref obj index) index
		   (if parental-setter
		       (setf-lambda (aref obj index))))))))

(defun traverse-by-json-pointer (obj parsed-pointer make-setter?)
  "Traverses an object with a parsed json-pointer, and returns three
values: a referred object, existence (boolean), and a closure can be
used as a setter."
  (let ((value obj)
	(exists? t)
	(setter
	 (if make-setter?
	     (thunk-lambda
	       (error 'json-pointer-access-error
		      :format-control "Setting to root object (~A) is not supported"
		      :format-arguments (list obj))))))
    (loop for (rtoken . next) on parsed-pointer
       do (setf (values value exists? setter)
		(traverse-by-reference-token value rtoken setter))
       while next)
    (values value exists? setter)))
