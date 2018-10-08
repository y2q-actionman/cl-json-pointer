(cl:defpackage #:cl-json-pointer/synonyms
  (:nicknames :cljsp)		      ; I worry about name conflict...
  (:documentation "Provide synonyms of cl-json-pointer package, for convenience.")
  (:use :cl #:cl-json-pointer)
  (:shadow #:get #:set #:delete #:remove)
  (:export
   #:json-pointer-error
   #:parse #:get #:exists-p #:set #:add #:delete #:remove #:update #:deletef))

(in-package :cl-json-pointer/synonyms)

(declaim (inline parse get exists-p set add delete remove))

(defun parse (obj &rest args &key &allow-other-keys)
  (apply #'parse-json-pointer obj args))

(defun get (obj pointer &rest args &key &allow-other-keys)
  (apply #'get-by-json-pointer obj pointer args))

(defun exists-p (obj pointer &rest args &key &allow-other-keys)
  (apply #'exists-p-by-json-pointer obj pointer args))

(defun set (obj pointer value &rest args &key &allow-other-keys)
  (apply #'set-by-json-pointer obj pointer value args))

(defun add (obj pointer value &rest args &key &allow-other-keys)
  (apply #'add-by-json-pointer obj pointer value args))

(defun delete (obj pointer &rest args &key &allow-other-keys)
  (apply #'delete-by-json-pointer obj pointer args))

(defun remove (obj pointer &rest args &key &allow-other-keys)
  (apply #'remove-by-json-pointer obj pointer args))

(define-setf-expander get (obj pointer &rest args &key &allow-other-keys &environment env)
  (get-setf-expansion `(get-by-json-pointer ,obj ,pointer ,@args) env))

(defmacro update (obj pointer value &rest keyargs)
  `(update-by-json-pointer ,obj ,pointer ,value ,@keyargs))

(defmacro deletef (obj pointer &rest keyargs)
  `(deletef-by-json-pointer ,obj ,pointer ,@keyargs))
