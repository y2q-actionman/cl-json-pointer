(in-package :cl-json-pointer)

(defconstant +last-nonexistent-element+
  '-
  "A symbol indicates 'the (nonexistent) member after the last array element', denoted by '-'")

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
			 :element-type 'character :adjustable t :fill-pointer 0))
	(tokens nil)
	(lne-count 0))
    (declare (type string buf)
	     (dynamic-extent buf)
	     (type integer lne-count))
    (flet ((push-reference-token ()
	     (when (string= buf +last-nonexistent-element+)
	       (incf lne-count))
	     (push (copy-seq buf) tokens)
	     (setf (fill-pointer buf) 0)))
      (loop with parsing-escape-token? of-type boolean = nil
	 for c of-type (or character symbol) = (read-char stream nil :eof)
	 if parsing-escape-token?
	 do (case c
	      (#\0 (vector-push-extend #\~ buf))
	      (#\1 (vector-push-extend #\/ buf))
	      (otherwise
	       (error 'json-pointer-syntax-error
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
    (values (nreverse tokens) lne-count)))

(defun parse-json-pointer (string &key (start 0) (end (length string))
				    (accept-uri-fragment t))
  (with-input-from-string (in string :start start :end end)
    (parse-json-pointer-from-stream in :accept-uri-fragment accept-uri-fragment)))
