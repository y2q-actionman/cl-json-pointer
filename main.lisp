(in-package :cl-json-pointer)

(define-condition json-pointer-syntax-error (simple-error)
  ())

(define-condition json-pointer-not-found-error (simple-error)
  ())

(defconstant +parse-json-pointer-default-buffer-length+ 16)

(defun parse-json-pointer-main-loop (string start end)
  (declare (type integer start end))
  (let ((ret ())
	(buf (make-array +parse-json-pointer-default-buffer-length+
			 :element-type 'character :adjustable t :fill-pointer 0))
	(parsing-escape-token? nil))
    (declare (type string buf)
	     (dynamic-extent buf)
	     (type boolean parsing-escape-token?))
    (flet ((push-reference-token ()
	     (when parsing-escape-token?
	       (error 'json-pointer-syntax-error
		      :format-control "too short escape token"))
	     (push (copy-seq buf) ret)
	     (setf (fill-pointer buf) 0)))
      (loop for i of-type integer from (1+ start) below end
	 as c of-type character = (char string i)

	 if (char= c #\/)
	 do (push-reference-token)
	 else if parsing-escape-token?
	 do (case c
	      (#\0 (vector-push-extend #\~ buf))
	      (#\1 (vector-push-extend #\/ buf))
	      (otherwise
	       (error 'json-pointer-syntax-error
		      :format-control "bad char as escape: ~C"
		      :format-arguments (list c))))
	   (setf parsing-escape-token? nil)
	 else if (char= #\~ c)
	 do (setf parsing-escape-token? t)
	 else
	 do (vector-push-extend c buf)
	 finally
	   (push-reference-token)))
    (nreverse ret)))

(defun parse-json-pointer (string &key (start 0) (end (length string))
				    (accept-uri-fragment t))
  (declare (type boolean accept-uri-fragment))
  ;; prefix and length check
  (when (zerop (- end start))
    (return-from parse-json-pointer ()))
  (let ((char0 (char string start)))
    (when (and accept-uri-fragment
	       (char= char0 #\#))
      (return-from parse-json-pointer
	(parse-json-pointer string :start (1+ start) :end end :accept-uri-fragment nil)))
    (unless (char= char0 #\/)
      (error 'json-pointer-syntax-error
	     :format-control "bad char as root: ~C"
	     :format-arguments (list char0))))
  ;; main loop
  (parse-json-pointer-main-loop string start end))


(defconstant +last-nonexistent-element+
  '+last-nonexistent-element+)
;; I think most-negative-fixnum is able to be used, but unsafe.. 

(defun read-reference-token-as-index (reference-token) ; raises parse-error
  (if (string= reference-token "-")
      +last-nonexistent-element+
      (handler-case (parse-integer reference-token)
	(parse-error () nil))))
  

(defun traverse-json (parsed-json parsed-pointer)
  (loop with obj = parsed-json
     for ptr in parsed-pointer
     ;; do (format t "~&obj ~S~% ptr ~S~%" obj ptr)
     do (typecase obj
	  (list
	   ;; 1. as alist
	   (let ((entry (assoc ptr obj :test #'string=)))
	     (unless entry
	       (error 'json-pointer-not-found-error
		      :format-control "obj ~A does not have '~A' member"
		      :format-arguments (list obj ptr)))
	     (setf obj (cdr entry)))
	   ;; 2. as (ordinal) list
	   ;; (TODO)
	   ;; (3. as plist -- required?)
	   )
	  (standard-object
	   ;; cl-json:fluid-object can be treated here.
	   ;; TODO: get slot list by MOP.
	   ;; TODO: support structure-object?
	   (progn))
	  (array
	   ;; TODO: support single '-' as the non-existent last element.
	   (let* ((ptr-index
		   (handler-case (read-reference-token-as-index ptr)
		     (error ()
		       (error 'json-pointer-not-found-error
			      :format-control "pointer ~A cannot be read as index (obj is ~A)"
			      :format-arguments (list ptr obj)))))
		  (array-len (length obj))
		  (index (if (>= ptr-index 0)
			     ptr-index
			     (- array-len ptr-index))))
	     (unless (and (<= 0 index) (< index array-len))
	       (error 'json-pointer-not-found-error
		      :format-control "Index ~A (pointer ~A) is out-of-index from ~A"
		      :format-arguments (list index ptr obj)))
	     (setf obj (aref obj index))))
	  (t
	   (error 'json-pointer-not-found-error
		  :format-control "obj ~A is not an array or an object (pointer is ~A)"
		  :format-arguments (list obj ptr))))
     finally
       (return obj)))

(defgeneric get-by-json-pointer (json-obj json-ptr))
(defgeneric set-by-json-pointer (json-obj json-ptr))
