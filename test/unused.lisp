(in-package :cl-json-pointer)

(defmacro define-this-source-pathname-variable (name)
  `(progn
     (eval-when (:compile-toplevel)
       (defparameter ,name *compile-file-truename*))
     (eval-when (:load-toplevel)
       (defvar ,name *load-truename*))
     (eval-when (:execute)
       (defvar ,name))))

(define-this-source-pathname-variable *this-file-path*)

(defparameter *rfc6901-example-path*
  (merge-pathnames "rfc6901-example.json"
		   *this-file-path*))
