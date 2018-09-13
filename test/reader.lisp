(in-package :cl-json-pointer/test)

(defun |#{-reader| (stream char n)
  (declare (ignore n))
  (with-output-to-string (out)
    (write-char char out)
    (loop with next-level = 1
       for c = (read-char stream t :eof t)
       do (write-char c out)
       when (and (char= c #\})
		 (zerop (decf next-level)))
       do (loop-finish))))

(defreadtable cjp-test-syntax
  (:merge :standard)
  (:dispatch-macro-char  #\# #\{ '|#{-reader|))
