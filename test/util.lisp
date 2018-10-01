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

;;; for multiple json libraries

(defvar *json-readers* nil)

(defvar *current-json-reader* nil)
(defvar *current-json-reader-array-type* nil)

(defun read-json-string (string)
  (check-type *current-json-reader* (or symbol function))
  (funcall *current-json-reader* string))

(define-constant +read-array-type-check+
  "[1]"
  :test #'equal)

(defmacro current-json-reader-array-etypecase (&body clauses)
  (loop with current-type = (gensym)
     for (type . body) in clauses
     collect `((subtypep ,current-type ',type) ,@body) into ex-clauses
     finally
       (return `(let ((,current-type
		       (type-of (read-json-string +read-array-type-check+))))
		  (cond ,@ex-clauses
			(t
			 (error "Unexpected type ~A for 'current-json-array-reader-etypecase'"
				,current-type)))))))

(defun run ()				; test entry point
  (loop for func in *json-readers*
     do (format *trace-output* "~&testing on ~A~%" func)
     always (let ((*current-json-reader* func)
		  (*current-json-reader-array-type* (read-json-string-array-type)))
	      (1am:run))))
