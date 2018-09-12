(in-package :cl-json-pointer)

(defconstant +last-nonexistent-element+
  '+last-nonexistent-element+
  "A placeholder indicates 'the (nonexistent) member after the last array element', denoted by '-'")

(defun read-reference-token-as-index (reference-token)
  (cond ((string= reference-token "-")
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


(defgeneric traverse-by-reference-token (obj rtoken &optional make-setter)
  (:documentation "Traverses an object with a reference token, and returns three values: a referred object, existence (boolean), and a closure can be used as a setter.")
  (:method (obj (rtoken null) &optional make-setter)
    ;; bottom case 1 -- refers an object with an empty token.
    (values obj
	    obj
	    (if make-setter
		(error 'json-pointer-not-found-error
		       :format-control "setting with an empty reference token is not supported"))))
  (:method (obj rtoken &optional make-setter)
    ;; bottom case 2 -- refers an unsupprted type object.
    (declare (ignore make-setter))
    (error 'json-pointer-not-found-error
	   :format-control "obj ~A is not an array or an object (pointer is ~A)"
	   :format-arguments (list obj rtoken))))

(defun alist-like-p (list)
  (every #'consp list))

(defvar *traverse-consider-plist* nil
  ;; I think there is no way to define `plist-like-p', because plist
  ;; does not restricted. Whether its keys are compared by `eq'
  ;; (http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm),
  ;; I think I should not assume the keys are always a symbol.
  "If this is T, cl-json-pointer considers plists at traversaling")

(defun string=-no-error (string1 string2 &rest string=-args)
  (ignore-errors
    (apply #'string= string1 string2 string=-args)))

(defmacro setf-lambda (access-form &key (key 'identity))
  `(lambda (x) (setf ,access-form (,key x))))

(defun make-list-tail-adder (list)
  ;; TODO: what to do if this is an empty list? (need a parental setter?)
  (setf-lambda (cdr (last list)) :key list))

(defmethod traverse-by-reference-token ((obj list) rtoken &optional make-setter)
  ;; As an alist
  (when (alist-like-p obj)
    (when-let ((entry (assoc rtoken obj :test #'string=-no-error)))
      (return-from traverse-by-reference-token
	(values (cdr entry)
		entry
		(if make-setter
		    (setf-lambda (cdr entry)))))))
  ;; As a plist (required?)
  (when *traverse-consider-plist*
    (loop for plist-head on obj by #'cddr
       as (k v) = plist-head
       when (string=-no-error k rtoken) ; plist often uses `eq', but we use `string='.
       do (return-from traverse-by-reference-token
	    (values v
		    plist-head
		    (if make-setter
			(setf-lambda (cadr plist-head)))))))
  ;; As a (ordinal) list
  (when-let ((index (ignore-errors
		      (read-reference-token-as-index rtoken))))
    (cond ((eq index +last-nonexistent-element+)
	   (return-from traverse-by-reference-token
	     (values nil
		     nil
		     (if make-setter
			 (make-list-tail-adder obj)))))
	  ((or (< index 0) (<= (length obj) index))
	   (error 'json-pointer-not-found-error
		  :format-control "Index ~A (pointer ~A) is out-of-index from ~A"
		  :format-arguments (list index rtoken obj)))
	  (t
	   (let ((this-cons (nthcdr index obj)))
	     (return-from traverse-by-reference-token
	       (values (car this-cons)
		       this-cons
		       (if make-setter
			   (setf-lambda (car this-cons)))))))))
  ;; Unfortunately..
  (error 'json-pointer-not-found-error
	 :format-control "obj ~A does not have '~A' member"
	 :format-arguments (list obj rtoken)))

(defun compare-string-by-case (a b &optional (case (readtable-case *readtable*)))
  (ecase case
    ((:upcase :downcase) (string-equal a b))
    ((:preserve :invert) (string= a b))))

(defmethod traverse-by-reference-token ((obj standard-object) rtoken &optional make-setter)
  ;; cl-json:fluid-object can be treated here.
  (loop with class = (class-of obj)
     for slot in (class-slots class)
     as slot-name = (slot-definition-name slot)
     when (compare-string-by-case rtoken slot-name)
     return
       (let ((bound? (slot-boundp-using-class class obj slot)))
	 (values (slot-value-using-class class obj slot)
		 bound?
		 (if make-setter
		     (setf-lambda (slot-value-using-class class obj slot))))))
  ;; TODO: support structure-object?
  ;; TODO: support condition-object?
  )

(defmethod traverse-by-reference-token ((obj hash-table) rtoken &optional make-setter)
  (multiple-value-bind (value exists?)
      (gethash rtoken obj)
    (values value
	    exists?
	    (if make-setter
		(setf-lambda (gethash rtoken obj))))))

(defun make-array-tail-adder (array)
  ;; TODO: what to do if not a fill-pointered vector?
  (lambda (x)
    (if (adjustable-array-p array)
	(vector-push-extend x array)
	(vector-push x array))))

(defmethod traverse-by-reference-token ((obj array) rtoken &optional make-setter)
  (let ((obj-len (length obj))
	(index (read-reference-token-as-index rtoken)))
    (cond ((eq index +last-nonexistent-element+)
	   (values nil
		   nil
		   (if make-setter
		       (make-array-tail-adder obj))))
	  ((or (< index 0) (<= obj-len index))
	   (error 'json-pointer-not-found-error
		  :format-control "Index ~A (pointer ~A) is out-of-index from ~A"
		  :format-arguments (list index rtoken obj)))
	  (t
	   (values (aref obj index)
		   index
		   (if make-setter
		       (setf-lambda (aref obj index))))))))

(defun traverse-by-json-pointer (parsed-json parsed-pointer)
  "Traverses an object with a parsed json-pointer, and returns three values: a referred object, existence (boolean), and a closure can be used as a setter."
  (loop for obj = parsed-json then (traverse-by-reference-token obj rtoken)
     for (rtoken . rest-ptr) on parsed-pointer
     ;; do (format t "~&obj ~S~% rtoken ~S, rest-ptr ~S~%" obj rtoken rest-ptr)
     finally (return obj)))
