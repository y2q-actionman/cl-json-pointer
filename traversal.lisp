(in-package :cl-json-pointer)

(defmacro setf-lambda (access-form &key (key 'identity))
  "Used for a simple setter."
  `(lambda (x) (setf ,access-form (,key x))))

(defun read-reference-token-as-index (reference-token)
  (cond ((string= reference-token +last-nonexistent-element+)
	 +last-nonexistent-element+)
	((and (> (length reference-token) 1)
	      (char= (char reference-token 0) #\0))
	 (error 'json-pointer-not-found-error
		:format-control "reference token (~A) should not start with 0 as index"
		:format-arguments (list reference-token)))
	(t
	 (handler-case (parse-integer reference-token)
	   (error ()
	     (error 'json-pointer-not-found-error
		    :format-control "reference token (~A) cannot be read as index"
		    :format-arguments (list reference-token)))))))


(defgeneric traverse-by-reference-token (obj rtoken make-setter? parental-setter)
  (:documentation "Traverses an object with a reference token, and returns three values: a referred object, existence (boolean), and a closure can be used as a setter."))

(defmethod traverse-by-reference-token (obj rtoken make-setter? parental-setter)
  ;; bottom case 1 -- refers an unsupported type object.
  (declare (ignore make-setter? parental-setter))
  (error 'json-pointer-not-found-error
	 :format-control "obj ~A is not an array or an object (pointer is ~A)"
	 :format-arguments (list obj rtoken)))

(defmethod traverse-by-reference-token (obj (rtoken null) make-setter? parental-setter)
  ;; bottom case 2 -- refers an object with an empty token.
  (declare (ignore parental-setter))
  (values obj
	  obj
	  (if make-setter?
	      (error 'json-pointer-access-error
		     :format-control "setting with an empty reference token is not supported"))))


(defun string=-no-error (string1 string2 &rest string=-args)
  (ignore-errors
    (apply #'string= string1 string2 string=-args)))


(defun traverse-alist-by-reference-token (alist rtoken make-setter? parental-setter)
  ;; accepts `nil' as alist.
  (if-let ((entry (assoc rtoken alist :test #'string=-no-error)))
    (values (cdr entry)
	    entry
	    (if make-setter?
		(setf-lambda (cdr entry))))
    (values nil
	    nil
	    (if make-setter?
		(lambda (x)
		  (setf alist (acons rtoken x alist))
		  (funcall parental-setter alist))))))

(defun traverse-plist-by-reference-token (plist rtoken make-setter? parental-setter)
  ;; accepts `nil' as plist.
  (loop for plist-head on plist by #'cddr
     as (k v) = plist-head
     when (string=-no-error k rtoken) ; plist often uses `eq', but we use `string='.
     return (values v
		    plist-head
		    (if make-setter?
			(setf-lambda (cadr plist-head))))
     finally
       (return (values nil
		       nil
		       (if make-setter?
			   (lambda (x)
			     (setf plist (list* rtoken x plist))
			     (funcall parental-setter plist)))))))

(defun traverse-ordinal-list-by-reference-token (list index make-setter? parental-setter)
  (if (eq index +last-nonexistent-element+)
      (values nil
	      nil
	      (if make-setter?
		  (lambda (x)
		    (if list
			(nconc list (list x))
			(funcall parental-setter (setf list (list x)))))))
      (if-let ((this-cons (nthcdr index list)))
	(values (car this-cons)
		this-cons
		(if make-setter?
		    (setf-lambda (car this-cons))))
	(error 'json-pointer-not-found-error
	       :format-control "Index ~A is out-of-index from ~A"
	       :format-arguments (list index list)))))


(defun alist-like-p (list)
  (every #'consp list))

(defun plist-like-p (list)
  (loop for (k nil) on list by #'cddr
     always (symbolp k)))

(defvar *traverse-consider-plist* nil
  ;; I think there is no way to define good `plist-like-p', because plist
  ;; does not restricted. Whether its keys are compared by `eq'
  ;; (http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm),
  ;; I think I should not assume the keys are always a symbol.
  "If this is T, cl-json-pointer considers plists at traversaling.")

(defmethod traverse-by-reference-token ((obj list) rtoken make-setter? parental-setter)
  (if-let ((index (ignore-errors
		    (read-reference-token-as-index rtoken))))
    ;; rtoken is ambiguous with index.
    (cond ((alist-like-p obj)
	   (traverse-alist-by-reference-token obj rtoken make-setter? parental-setter))
	  ((and *traverse-consider-plist*
		(plist-like-p obj))
	   (traverse-plist-by-reference-token obj rtoken make-setter? parental-setter))
	  (t
	   (traverse-ordinal-list-by-reference-token obj index make-setter? parental-setter)))
    ;; I assume `rtoken' is not index, so considers alist (or plist).
    (if (and *traverse-consider-plist*
	     (plist-like-p obj))
	(traverse-plist-by-reference-token obj rtoken make-setter? parental-setter)
	(traverse-alist-by-reference-token obj rtoken make-setter? parental-setter))))

(defmethod traverse-by-reference-token ((obj null) rtoken make-setter? parental-setter)
  ;; empty. this is problematic for setting.
  (if-let ((index (ignore-errors
		    (read-reference-token-as-index rtoken))))
    ;; rtoken is ambiguous with index.
    (traverse-ordinal-list-by-reference-token obj index make-setter? parental-setter)
    ;; I assume `rtoken' is not index, so considers alist (or plist).
    (traverse-alist-by-reference-token obj rtoken make-setter? parental-setter)))

(defun compare-string-by-case (a b &optional (case (readtable-case *readtable*)))
  (ecase case
    ((:upcase :downcase) (string-equal a b))
    ((:preserve :invert) (string= a b))))

(defmethod traverse-by-reference-token ((obj standard-object) rtoken make-setter? parental-setter)
  ;; cl-json:fluid-object can be treated here.
  (loop with class = (class-of obj)
     for slot in (class-slots class)
     as slot-name = (slot-definition-name slot)
     when (compare-string-by-case rtoken slot-name)
     return
       (let ((bound? (slot-boundp-using-class class obj slot)))
	 (values (if bound?
		     (slot-value-using-class class obj slot))
		 bound?
		 (if make-setter?
		     (setf-lambda (slot-value-using-class class obj slot)))))
     finally
       (return
	 (values nil
		 nil
		 (if make-setter?
		     (lambda (x)
		       (declare (ignore x))
		       (error 'json-pointer-access-error
			      :format-control "object ~A does not have '~A' slot"
			      :format-arguments (list obj rtoken)))))))
  ;; TODO: support structure-object?
  ;; TODO: support condition-object?
  )

(defmethod traverse-by-reference-token ((obj hash-table) rtoken make-setter? parental-setter)
  (declare (ignore parental-setter))
  (multiple-value-bind (value exists?)
      (gethash rtoken obj)
    (values value
	    exists?
	    (if make-setter?
		(setf-lambda (gethash rtoken obj))))))

(defun make-array-tail-adder (array parental-setter)
  (lambda (x)
    (check-type array (array * (*)))
    (let ((adjustable? (adjustable-array-p array))
	  (has-fill-pointer? (array-has-fill-pointer-p array)))
      (cond ((and adjustable?
		  has-fill-pointer?)
	     (vector-push-extend x array))
	    ((and has-fill-pointer?
		  (or (pprint 2) t)
		  (vector-push x array))) ; uses `vector-push' result as condition.
	    (t
	     (let* ((original-length (length array))
		    (new-array (make-array (* 2 original-length)
					   :adjustable t
					   :fill-pointer original-length)))
	       (replace new-array array)
	       (setf array new-array))
	     (funcall parental-setter array) ; FIXME: add a type check??
	     (vector-push x array))))))

(defmethod traverse-by-reference-token ((obj array) rtoken make-setter? parental-setter)
  (let ((index (read-reference-token-as-index rtoken)))
    (cond ((eq index +last-nonexistent-element+)
	   (values nil
		   nil
		   (if make-setter?
		       (make-array-tail-adder obj parental-setter))))
	  ((not (array-in-bounds-p obj index))
	   (error 'json-pointer-not-found-error
		  :format-control "Index ~A (pointer ~A) is out-of-index from ~A"
		  :format-arguments (list index rtoken obj)))
	  (t
	   (values (aref obj index)
		   index
		   (if make-setter?
		       (setf-lambda (aref obj index))))))))

(defun traverse-by-json-pointer (obj parsed-pointer &key (always-make-setter nil))
  "Traverses an object with a parsed json-pointer, and returns three
values: a referred object, existence (boolean), and a closure can be
used as a setter."
  (loop with last-setter = nil
     for (rtoken . next?) on parsed-pointer
     do (multiple-value-bind (value exists? setter)
	    (traverse-by-reference-token obj rtoken
					 (or always-make-setter (not next?))
					 last-setter)
	  (when (null next?)
	    (return
	      (values value exists? setter)))
	  (setf obj value
		last-setter setter))))
