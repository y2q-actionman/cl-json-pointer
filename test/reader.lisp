(in-package :cl-json-pointer/test)

;;; cl-json
;; TODO: use fluid-object 

;; specially crafted cl-json's one. I make it default.
(defun read-json-string/cl-json-crafted (string)
  (cl-json:bind-custom-vars (:array-type 'vector)
    (let ((cl-json:*json-identifier-name-to-lisp* #'identity))
      (cl-json:decode-json-from-string string))))

(unless *current-json-reader*
  (setf *current-json-reader* 'read-json-string/cl-json-crafted))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'cl-json:decode-json-from-string *json-readers*)
  (pushnew 'read-json-string/cl-json-crafted *json-readers*))

;;; st-json

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'st-json:read-json-from-string *json-readers*))

;;; yason

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'yason:parse *json-readers*))

;; TODO: use variables
;; (*parse-json-arrays-as-vectors*)
;; (*parse-json-booleans-as-symbols*) 
;; (*parse-json-null-as-keyword*)
;; (*parse-object-key-fn*)
