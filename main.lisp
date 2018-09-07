(in-package :cl-json-pointer)

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
