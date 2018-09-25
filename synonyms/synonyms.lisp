(cl:defpackage #:cl-json-pointer/synonyms
  (:nicknames :cljsp)		      ; I worry about name conflict...
  (:documentation "Provide synonyms of cl-json-pointer package, for convenience.")
  (:use :cl #:cl-json-pointer)
  (:shadow #:get #:set #:delete #:remove)
  (:export
   #:parse #:get #:exists-p #:set #:add #:delete #:remove #:update))

(in-package :cl-json-pointer/synonyms)

(declaim (inline parse get exists-p set add delete remove))

(defun parse (obj &rest args &key &allow-other-keys)
  (apply #'parse-json-pointer obj args))

(defun get (obj pointer)
  (get-by-json-pointer obj pointer))

(defun exists-p (obj pointer)
  (exists-p-by-json-pointer obj pointer))

(defun set (obj pointer value)
  (set-by-json-pointer obj pointer value))

(defun add (obj pointer value)
  (add-by-json-pointer obj pointer value))

(defun delete (obj pointer)
  (delete-by-json-pointer obj pointer))

(defun remove (obj pointer)
  (remove-by-json-pointer obj pointer))

(define-setf-expander get (obj pointer &environment env)
  (get-setf-expansion `(get-by-json-pointer ,obj ,pointer) env))

(defmacro update (obj pointer value)
  `(update-by-json-pointer ,obj ,pointer ,value))
