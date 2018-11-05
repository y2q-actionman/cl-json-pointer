(cl:defpackage #:cl-json-pointer/trush
  (:use :cl #:cl-json-pointer
	#:alexandria)
  (:export))

(in-package :cl-json-pointer/trush)

;;; deleted from utils

(defun remove-nth (list n)
  (check-type n (integer 0))
  (loop with heads = nil
     for i from 0 below n
     do (push (pop list) heads)
     while list
     finally
       (return (nreconc heads (cdr list)))))

(defun delete-nth (list n)
  (check-type n (integer 0))
  (if (zerop n)
      (cdr list)
      (loop for c on list
	 for i from 0 below (1- n)
	 finally
	   (setf (cdr c) (cddr c))
	   (return list))))

(defun string-invert-case (string &key (start 0) (end nil))
  (loop with end = (or end (length string))
     with ret = (make-array (- end start) :element-type 'character)
     for i from start below end
     as c = (char string i)
     for j from 0
     do (setf (char ret j)
	      (cond ((upper-case-p c) (char-downcase c))
		    ((lower-case-p c) (char-upcase c))
		    (t c)))
     finally (return ret)))

(defun intern-with-readtable-case (name &key (case (readtable-case *readtable*))
					  (package *package*))
  (intern (ecase case
	    (:upcase (string-upcase name))
	    (:downcase (string-downcase name))
	    (:preserve name)
	    (:invert (string-invert-case name)))
	  package))


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
