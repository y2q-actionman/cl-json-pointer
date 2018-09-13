(in-package :cl-json-pointer/test)

(defmacro with-json-pointer-style (() &body body)
  `(cl-json:bind-custom-vars (:array-type 'vector)
     (let ((cl-json:*json-identifier-name-to-lisp* #'identity))
       ,@body)))

(defun read-json-file (path)
  (with-open-file (stream path :direction :input)
    (with-json-pointer-style ()
      (cl-json:decode-json stream))))

(defun read-json-string (string)
  (with-json-pointer-style ()
    (cl-json:decode-json-from-string string)))
