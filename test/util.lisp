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

(defvar *json-reader-alist* nil
  "See `run'")

(defvar *current-json-reader* nil
  "See `with-current-json-reader'")

(defvar *current-array-type* nil
  "See `with-current-json-reader'")

(defun read-json-string (string)
  "Reads STRING using `*current-json-reader*'."
  (check-type *current-json-reader* (or symbol function))
  (funcall *current-json-reader* string))

(define-constant +array-type-check+
  "[1]"
  :test #'equal
  :documentation "Used for checking how JSON libs treat arrays. See `with-current-json-reader'.")

(defmacro esubtypecase ((type-var) &body clauses)
  "Like `etypecase', but uses `subtypep' for type comparison."
  (loop with current-type = (gensym)
     for (type . body) in clauses
     collect `((subtypep ,current-type ',type) ,@body) into ex-clauses
     finally
       (return `(let ((,current-type ,type-var))
		  (cond ,@ex-clauses
			(t
			 (error "Unexpected type ~A for 'esubtypecase'"
				,current-type)))))))

(defmacro with-current-json-reader ((flavor reader-func) &body body)
  "Binds `*current-json-reader*', `*current-array-type*', and
`*json-object-flavor*' referring READER-FUNC, and runs BODY."
  (let ()
    `(let* ((*current-json-reader* ,reader-func)
	    (*current-array-type* (type-of (read-json-string +array-type-check+)))
	    (*json-object-flavor* ,flavor))
       ,@body)))

(defun print-test-heading ()
  (format t "~&testing on ~A:~A~&  JSON object flavor ~A~&  JSON array = ~A~%"
	  (package-name (symbol-package *current-json-reader*))
	  *current-json-reader*
	  *json-object-flavor* *current-array-type*))

(defun run (&optional (reader-alist *json-reader-alist*))	; test entry point
  "Runs `1am:run' with changing JSON backend based on `*json-reader-alist*'"
  (loop with shuffled = (alexandria:shuffle (copy-list reader-alist))
     for (flavor . func) in shuffled
     do (with-current-json-reader (flavor func)
	  (print-test-heading)
	  (1am:run))))
