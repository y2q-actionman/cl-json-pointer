(in-package :cl-json-pointer/test)

;;; This code does not consider escaped parens.
(defun parens-reader (stream start-char n)
  (declare (ignore n))
  (let ((end-char
	 (ecase start-char
	   (#\{ #\})
	   (#\[ #\]))))
    (with-output-to-string (out)
      (write-char start-char out)
      (loop with nest-level = 1
	 for c = (read-char stream t :eof t)
	 do (write-char c out)
	   (cond ((eql c start-char)
		  (incf nest-level))
		 ((eql c end-char)
		  (decf nest-level)
		  (when (zerop nest-level)
		    (loop-finish))))))))

(defreadtable cjp-test-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\{ 'parens-reader)
  (:dispatch-macro-char #\# #\[ 'parens-reader))

(defmacro assert-condition (&body body)
  (with-gensyms (ret condition)
    `(multiple-value-bind (,ret ,condition)
	 (ignore-errors (progn ,@body))
       (assert (and (null ,ret) ,condition)))))

;;; TODO: FIXME: include it to main library?
(define-modify-macro update-by-json-pointer (pointer value)
  set-by-json-pointer)

(define-modify-macro update! (pointer value)
  cljsp:set)

(define-modify-macro delete! (pointer)
  cljsp:delete)
