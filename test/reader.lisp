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
  (pushnew (cons t 'cl-json:decode-json-from-string)
	   *json-reader-alist*)
  (pushnew (cons :cl-json 'cl-json:decode-json-from-string)
	   *json-reader-alist*))

;;; st-json

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (cons :st-json 'st-json:read-json-from-string)
	   *json-reader-alist*))

;;; yason

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (cons :yason 'yason:parse)
	   *json-reader-alist*))

;; TODO: use variables
;; (*parse-json-arrays-as-vectors*)
;; (*parse-json-booleans-as-symbols*) 
;; (*parse-json-null-as-keyword*)
;; (*parse-object-key-fn*)

;;; jsown

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (cons :jsown 'jsown:parse)
	   *json-reader-alist*))

;;; com.gigamonkeys.json

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (cons :com.gigamonkeys.json 'com.gigamonkeys.json:parse-json)
	   *json-reader-alist*))
