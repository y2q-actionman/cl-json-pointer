(cl:defpackage #:cl-json-pointer/trush
  (:use :cl #:cl-json-pointer
	#:alexandria #:closer-mop)
  (:export))

(in-package :cl-json-pointer/trush)


;;; deleted from test codes.

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


;;; deleted from traversal.lisp

(defmacro aconsf-internal (ref key value)
  `(acons ,key ,value ,ref))

(define-modify-macro aconsf (key value)
  aconsf-internal)

(defmacro list*-f-internal (ref &rest values)
  `(list* ,@values ,ref))

(define-modify-macro list*-f (&rest values)
  list*-f-internal)

(defmacro error-thunk (error-datum &rest error-args)
  "Used for delaying errors."
  `(thunk-lambda (error ,error-datum ,@error-args)))

(defmacro add-to-tail* (list-var nil-handler &rest new-entries)
  `(let* ((null? (null ,list-var))
	  (obj-to-conc (list ,@new-entries))
	  (new-list (nconcf ,list-var obj-to-conc)))
     (when null?
       (funcall ,nil-handler new-list))
     new-list))

(defmacro setf-lambda (access-form)
  "Used for a simple setter."
  (with-gensyms (x)
    `(lambda (,x) (setf ,access-form ,x))))
