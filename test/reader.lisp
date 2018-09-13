(in-package :cl-json-pointer/test)

(defun |#{-reader| (stream char n)
  (declare (ignore n))
  (with-output-to-string (out)
    (write-char char out)
    (loop with nest-level = 1
       for c = (read-char stream t :eof t)
       do (write-char c out)
	 (case c
	   (#\{ (incf nest-level))
	   (#\} (decf nest-level)
		(when (zerop nest-level)
		  (loop-finish)))))))

(defreadtable cjp-test-syntax
  (:merge :standard)
  (:dispatch-macro-char  #\# #\{ '|#{-reader|))
