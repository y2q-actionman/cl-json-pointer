(in-package :cl-json-pointer/test)

(defun read-json-string/cl-json-crafted (string)
  (cl-json:bind-custom-vars (:array-type 'vector)
    (let ((cl-json:*json-identifier-name-to-lisp* #'identity))
      (cl-json:decode-json-from-string string))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'cl-json:decode-json-from-string *json-readers*)
  (pushnew 'read-json-string/cl-json-crafted *json-readers*)
  ;; I make it default..
  (unless *current-json-reader*
    (setf *current-json-reader* 'read-json-string/cl-json-crafted)))
