(in-package :cl-json-pointer)

(defconstant +end+
  '-
  "A symbol indicates 'the (nonexistent) member after the last array element', denoted by '-'")

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

;;; object key

(defgeneric intern-object-key (flavor rtoken)
  (:documentation "Interns `rtoken' as JSON object key, with JSON lib flavor of `flavor'")
  (:method (flavor (rtoken symbol))
    (declare (ignore flavor))
    rtoken)
  (:method (flavor (rtoken string))	; This case is ambigouns
    "Interns `rtoken' itself as JSON object key.
This is suitable for yason, st-json, jsown, json-streams, and com.gigamonkeys.json."
    (declare (ignore flavor))
    rtoken))

;;; Parser

(deftype parsed-json-pointer ()
  'list)

(defgeneric parse-json-pointer (obj &key start end accept-uri-fragment)
  (:documentation "Parses `pointer' to an internal representation"))

(defmethod parse-json-pointer (pointer &key &allow-other-keys)
  (error 'json-pointer-parse-error
	 :format-control "Unsupported type for parsing"
	 :format-arguments (list pointer)))

(defmethod parse-json-pointer ((pointer list) &key &allow-other-keys)
  ;; a short circuit for current `parsed-json-pointer' definition.
  pointer)

(defconstant +parse-json-pointer-default-buffer-length+ 16)

(defmethod parse-json-pointer ((stream stream) &key (accept-uri-fragment t) &allow-other-keys)
  ;; checks '#'
  (when accept-uri-fragment
    (case (peek-char nil stream nil)
      (#\# (read-char stream))	; accepts '#'
      (otherwise (progn))))
  ;; checks '/' at the beginning, and consume it here.
  (let ((char0 (read-char stream nil :eof)))
    (case char0
      (:eof ; I think RFC6901 says an empty string should be accepted.
       (return-from parse-json-pointer nil))
      (#\/
       (progn))				; ok
      (otherwise
       (error 'json-pointer-parse-error
	      :format-control "Not started by '/', appeared '~A'"
	      :format-arguments (list char0)))))
  ;; main loop
  (let ((buf (make-array +parse-json-pointer-default-buffer-length+
			 :element-type 'character :adjustable t :fill-pointer 0))
	(tokens nil))
    (declare (type string buf)
	     (dynamic-extent buf))
    (flet ((push-reference-token ()
	     (push (if (string= buf "-")
		       +end+
		       (copy-seq buf))
		   tokens)
	     (setf (fill-pointer buf) 0)))
      (loop with parsing-escape-token? of-type boolean = nil
	 for c of-type (or character symbol) = (read-char stream nil :eof)
	 if parsing-escape-token?
	 do (case c
	      (#\0 (vector-push-extend #\~ buf))
	      (#\1 (vector-push-extend #\/ buf))
	      (otherwise
	       (error 'json-pointer-parse-error
		      :format-control "bad char as escape: ~A"
		      :format-arguments (list c))))
	   (setf parsing-escape-token? nil)
	 else
	 do (case c
	      (:eof (push-reference-token)
		    (loop-finish))
	      (#\/ (push-reference-token))
	      (#\~ (setf parsing-escape-token? t))
	      (otherwise (vector-push-extend c buf)))))
    (nreverse tokens)))

(defmethod parse-json-pointer ((pointer string) &key (start 0) (end (length pointer))
						  (accept-uri-fragment t) &allow-other-keys)
  (with-input-from-string (in pointer :start start :end end)
    (parse-json-pointer in :accept-uri-fragment accept-uri-fragment)))
