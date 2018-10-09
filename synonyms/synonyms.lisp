(cl:defpackage #:cl-json-pointer/synonyms
  (:nicknames :cljsp)		      ; I worry about name conflict...
  (:documentation "Provide synonyms of cl-json-pointer package, for convenience.")
  (:use :cl #:cl-json-pointer)
  (:shadow #:get #:set #:delete #:remove)
  (:export
   #:json-pointer-error
   #:parse #:get #:exists-p #:set #:add #:delete #:remove #:update #:deletef))

(in-package :cl-json-pointer/synonyms)

(defmacro defsynonym-cljsp-func (name (func &rest required-args))
  `(progn (declaim (inline ,name))
	  (defun ,name (,@required-args &rest keyargs &key &allow-other-keys)
	    (apply #',func ,@required-args keyargs))))

(defsynonym-cljsp-func parse (parse-json-pointer obj))

(defsynonym-cljsp-func get (get-by-json-pointer obj pointer))

(defsynonym-cljsp-func exists-p (exists-p-by-json-pointer obj pointer))

(defsynonym-cljsp-func set (set-by-json-pointer obj pointer value))

(defsynonym-cljsp-func add (add-by-json-pointer obj pointer value))

(defsynonym-cljsp-func delete (delete-by-json-pointer obj pointer))

(defsynonym-cljsp-func remove (remove-by-json-pointer obj pointer))

(define-setf-expander get (obj pointer &rest args &key &allow-other-keys &environment env)
  (get-setf-expansion `(get-by-json-pointer ,obj ,pointer ,@args) env))

(defmacro update (obj pointer value &rest keyargs)
  `(update-by-json-pointer ,obj ,pointer ,value ,@keyargs))
 
(defmacro deletef (obj pointer &rest keyargs)
  `(deletef-by-json-pointer ,obj ,pointer ,@keyargs))
