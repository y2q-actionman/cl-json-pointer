(cl:defpackage #:cl-json-pointer/synonyms
  (:nicknames :cljsp)		      ; I worry about name conflict...
  (:documentation "Provide synonyms of cl-json-pointer package, for convenience.")
  (:use :cl #:cl-json-pointer)
  (:shadow #:get #:set #:delete #:remove)
  (:export
   #:json-pointer-error
   #:parse #:get #:exists-p #:set #:add #:delete #:remove #:update))

(in-package :cl-json-pointer/synonyms)

(declaim (inline parse get exists-p set add delete remove))

(defun parse (obj &rest args &key &allow-other-keys)
  (apply #'parse-json-pointer obj args))

(defun get (obj pointer &key (type t))
  (get-by-json-pointer obj pointer :type type))

(defun exists-p (obj pointer &key (type t))
  (exists-p-by-json-pointer obj pointer :type type))

(defun set (obj pointer value &key (type t))
  (set-by-json-pointer obj pointer value :type type))

(defun add (obj pointer value &key (type t))
  (add-by-json-pointer obj pointer value :type type))

(defun delete (obj pointer &key (type t))
  (delete-by-json-pointer obj pointer :type type))

(defun remove (obj pointer &key (type t))
  (remove-by-json-pointer obj pointer :type type))

(define-setf-expander get (obj pointer &key (type t) &environment env)
  (get-setf-expansion `(get-by-json-pointer ,obj ,pointer :type ,type) env))

;;; FIXME
#+ (or)
(defmacro update (obj pointer value &key (type t))
  `(update-by-json-pointer ,obj ,pointer ,value :type ,type))
