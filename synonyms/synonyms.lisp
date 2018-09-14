(cl:defpackage #:cl-json-pointer/synonyms
  (:nicknames :cljp/s)		      ; I worry about name conflict...
  (:documentation "Provide synonyms of cl-json-pointer package, for convenience.")
  (:use :cl #:cl-json-pointer)
  (:shadow #:set #:get)
  (:export
   #:parse #:get #:exists-p #:set))

(in-package :cl-json-pointer/synonyms)

(declaim (inline parse get exists-p set))

(defun parse (obj &rest args &key &allow-other-keys)
  (apply #'parse-json-pointer obj args))

(defun get (obj pointer)
  (get-by-json-pointer obj pointer))

(defun exists-p (obj pointer)
  (exists-p-by-json-pointer obj pointer))

(defun set (obj pointer value)
  (set-by-json-pointer obj pointer value))
