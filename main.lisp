(in-package :cl-json-pointer)

;;; TODO
#+ignore
(defclass parsed-json-pointer ()
  ((token-list :initarg :token-list :initform () :accessor pjp-token-list)))

(define-condition json-pointer-syntax-error (simple-error)
  ())

(define-condition json-pointer-not-found-error (simple-error)
  ())

(defconstant +parse-json-pointer-default-buffer-length+ 16)

(defun parse-json-pointer-from-stream (stream &key (accept-uri-fragment t))
  ;; checks '#'
  (when accept-uri-fragment
    (let ((char0 (read-char stream nil :eof)))
      (case char0
	(#\# (progn))			; '#' accepted
	(:eof (progn))			; do not unread EOF.
	(otherwise (unread-char char0 stream)))))
  ;; checks '/' at the beginning, and consume it here.
  (let ((char0 (read-char stream nil :eof)))
    (case char0
      (:eof ; I think RFC6901 says an empty string should be accepted.
       (return-from parse-json-pointer-from-stream nil))
      (#\/
       (progn))				; ok
      (otherwise
       (error 'json-pointer-syntax-error
	      :format-control "Not started by '/', appeared '~A'"
	      :format-arguments (list char0)))))
  ;; main loop
  (let ((buf (make-array +parse-json-pointer-default-buffer-length+
			 :element-type 'character :adjustable t :fill-pointer 0)))
    (declare (type string buf)
	     (dynamic-extent buf))
    (flet ((make-reference-token ()
	     (prog1 (copy-seq buf)
	       (setf (fill-pointer buf) 0))))
      (loop with parsing-escape-token? of-type boolean = nil
	 for c of-type (or symbol null) = (read-char stream nil :eof)
	 if parsing-escape-token?
	 do (case c
	      (#\0 (vector-push-extend #\~ buf))
	      (#\1 (vector-push-extend #\/ buf))
	      (otherwise
	       (error 'json-pointer-syntax-error
		      :format-control "bad char as escape: ~A"
		      :format-arguments (list c))))
	   (setf parsing-escape-token? nil)
	 else if (eq c :eof)
	 collect (make-reference-token)
	 and do (loop-finish)
	 else if (eq c #\/)
	 collect (make-reference-token)
	 else if (eq c #\~)
	 do (setf parsing-escape-token? t)
	 else
	 do (vector-push-extend c buf)))))
    
(defun parse-json-pointer (string &key (start 0) (end (length string))
				    (accept-uri-fragment t))
  (with-input-from-string (in string :start start :end end)
    (parse-json-pointer-from-stream in :accept-uri-fragment accept-uri-fragment)))


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
  (:method (obj rtoken &optional make-setter)
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

(defmethod traverse-by-reference-token ((obj list) rtoken &optional make-setter)
  ;; As an alist
  (when (alist-like-p obj)
    (ignore-errors			; TODO: I should use like `string=-no-error' one.
      (when-let ((entry (assoc rtoken obj :test #'string=)))
	(return-from traverse-by-reference-token
	  (values (cdr entry)
		  entry
		  (if make-setter
		      (named-lambda set-to-this-alist (x)
			(setf (cdr entry) x))))))))
  ;; As a plist (required?)
  (when *traverse-consider-plist*
    (ignore-errors			; TODO: see above.
      (loop for plist-head on obj by #'cddr
	 as (k v) = plist-head
	 when (string= k rtoken) ; plist often uses `eq', but we use `string='.
	 do (return-from traverse-by-reference-token
	      (values v
		      plist-head
		      (if make-setter
			  (named-lambda set-to-this-plist (x)
			    (setf (cadr plist-head) x))))))))
  ;; As a (ordinal) list
  (when-let ((index (ignore-errors
		      (read-reference-token-as-index rtoken))))
    (cond ((eq index +last-nonexistent-element+)
	   (return-from traverse-by-reference-token
	     (values nil
		     nil
		     (if make-setter
			 (named-lambda add-to-this-list-tail (x)
			   (setf (cdr (last obj)) (list x)))))))
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
			   (named-lambda set-to-this-list (x)
			     (setf (car this-cons) x)))))))))
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
		     (named-lambda set-to-this-slot (x)
		       (setf (slot-value-using-class class obj slot) x))))))
  ;; TODO: support structure-object?
  ;; TODO: support condition-object?
  )

(defmethod traverse-by-reference-token ((obj hash-table) rtoken &optional make-setter)
  (multiple-value-bind (value exists?)
      (gethash rtoken obj)
    (values value
	    exists?
	    (if make-setter
		(named-lambda set-to-this-hash-table (x)
		  (setf (gethash rtoken obj) x))))))

(defmethod traverse-by-reference-token ((obj array) rtoken &optional make-setter)
  (let ((obj-len (length obj))
	(index (read-reference-token-as-index rtoken)))
    (cond ((eq index +last-nonexistent-element+)
	   ;; TODO: what to do if not a fill-pointered vector?
	   (values nil
		   nil
		   (if make-setter
		       (named-lambda push-to-this-array (x)
			 (if (adjustable-array-p obj)
			     (vector-push-extend x obj)
			     (vector-push x obj))))))
	  ((or (< index 0) (<= obj-len index))
	   (error 'json-pointer-not-found-error
		  :format-control "Index ~A (pointer ~A) is out-of-index from ~A"
		  :format-arguments (list index rtoken obj)))
	  (t
	   (values (aref obj index)
		   index
		   (if make-setter
		       (named-lambda set-to-this-array (x)
			 (setf (aref obj index) x))))))))

(defun traverse-json (parsed-json parsed-pointer)
  (loop for obj = parsed-json then (traverse-by-reference-token obj rtoken)
     for (rtoken . rest-ptr) on parsed-pointer
     ;; do (format t "~&obj ~S~% rtoken ~S, rest-ptr ~S~%" obj rtoken rest-ptr)
     finally (return obj)))

(defgeneric get-by-json-pointer (json-obj json-ptr))
(defgeneric set-by-json-pointer (json-obj json-ptr))
