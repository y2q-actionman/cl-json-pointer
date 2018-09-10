(in-package :cl-json-pointer)


(defvar *rfc6901-example-keys*
  '(""
    "/foo"
    "/foo/0"
    "/"
    "/a~1b"
    "/c%d"
    "/e^f"
    "/g|h"
    "/i\\j"
    "/k\"l"
    "/ "
    "/m~0n"))

(defun test-parse-json-pointer ()
  (flet ((dt (cas)
	   (format t "~&case ~S, parsed ~S~%" cas (parse-json-pointer cas))))
    (loop for c in *rfc6901-example-keys*
       do (dt c))))


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

(defun test-traverse-json ()
  (let ((json (read-json-file *rfc6901-example-path*)))
    (flet ((dt (cas)
	     (let* ((ptr (parse-json-pointer cas))
		    (result (traverse-json json ptr)))
	       (format t "~&pointer ~S, parsed-pointer ~S, result ~S~%"
		       cas ptr result))))
      (loop for c in *rfc6901-example-keys*
	 do (dt c)))))


(defclass test-class ()
  ((hoge :initform 'hoge-value)))

(defun test-traverse-json-2 ()
  (let ((obj (make-instance 'test-class))
	(ptr "/hoge"))
    (traverse-json obj (parse-json-pointer ptr))))
