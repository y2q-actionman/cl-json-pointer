(in-package :cl-json-pointer/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; specially crafted cl-json's one. I make it default.
  (defun read-json-string/cl-json-crafted (string)
    (cl-json:bind-custom-vars (:array-type 'vector)
      (let ((cl-json:*json-identifier-name-to-lisp* #'identity))
	(cl-json:decode-json-from-string string))))
  (unless *current-json-reader*
    (setf *current-json-reader* 'read-json-string/cl-json-crafted)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'cl-json:decode-json-from-string *json-readers*)
  (pushnew 'read-json-string/cl-json-crafted *json-readers*)
  (pushnew 'st-json:read-json-from-string *json-readers*))
