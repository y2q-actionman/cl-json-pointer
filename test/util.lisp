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
(defvar *current-array-type* nil)
(defvar *current-object-type* nil)

(defun read-json-string (string)
  (check-type *current-json-reader* (or symbol function))
  (funcall *current-json-reader* string))

(define-constant +array-type-check+
  "[1]"
  :test #'equal)

(define-constant +object-type-check+
  "{\"a\": 1}"
  :test #'equal)

(defmacro current-json-reader-etypecase ((type-var) &body clauses)
  (loop with current-type = (gensym)
     for (type . body) in clauses
     collect `((subtypep ,current-type ',type) ,@body) into ex-clauses
     finally
       (return `(let ((,current-type ,type-var))
		  (cond ,@ex-clauses
			(t
			 (error "Unexpected type ~A for 'current-json-reader-etypecase'"
				,current-type)))))))

(defmacro with-current-json-reader ((func) &body body)
  `(let* ((*current-json-reader* ,func)
	  (*current-array-type* (type-of (read-json-string +array-type-check+)))
	  (*current-object-type* (type-of (read-json-string +object-type-check+))))
     ,@body))

(defun run (&optional (readers *json-readers*))	; test entry point
  (loop for func in readers
     do (with-current-json-reader (func)
	  (format t "~&testing on ~A:~A~& (JSON array = ~A, JSON object = ~A)~%"
		  (package-name (symbol-package *current-json-reader*))
		  *current-json-reader*
		  *current-array-type* *current-object-type*)
	  (1am:run))))
