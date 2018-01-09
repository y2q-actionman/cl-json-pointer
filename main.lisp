(in-package :cl-json-pointer)

(define-condition json-pointer-syntax-error (simple-error)
  ())

(define-condition json-pointer-not-found-error (simple-error)
  ())

(defparameter *parse-json-pointer-buffer-length* 16)

;; TODO: accept URI fragment marker
(defun parse-json-pointer (string)
  (let ((ret ())
	(string-len (length string))
	(buf (make-array *parse-json-pointer-buffer-length*
			 :element-type 'character :adjustable t :fill-pointer 0))
	(parsing-escape-token? nil))
    (when (zerop string-len)
      (return-from parse-json-pointer ()))
    (unless (char= (char string 0) #\/)
      (error 'json-pointer-syntax-error
	     :format-control "bad char as root: ~C"
	     :format-arguments (list (char string 0))))
    (flet ((push-reference-token ()
	     (when parsing-escape-token?
	       (error 'json-pointer-syntax-error
		      :format-control "too short escape token"))
	     (push (copy-seq buf) ret)
	     (setf (fill-pointer buf) 0)))
      (loop for i of-type fixnum from 1 below string-len
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

(defun traverse-json (parsed-json parsed-pointer)
  (loop with obj = parsed-json
     for ptr in parsed-pointer
     ;; do (format t "~&obj ~S~% ptr ~S~%" obj ptr)
     do (typecase obj
	  (list				; alist
	   (let ((entry (assoc ptr obj :test #'string=)))
	     (unless entry
	       (error 'json-pointer-not-found-error
		      :format-control "obj ~A does not have '~A' member"
		      :format-arguments (list obj ptr)))
	     (setf obj (cdr entry))))
	  (array
	   (let* ((ptr-index
		   (handler-case (parse-integer ptr)
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

(defmacro with-json-pointer-style (() &body body)
  `(cl-json:bind-custom-vars (:array-type 'vector)
     (let ((cl-json:*json-identifier-name-to-lisp* #'identity))
       ,@body)))
  

(defun read-json-file (path)
  (with-open-file (stream path :direction :input)
    (with-json-pointer-style ()
      (cl-json:decode-json stream))))
