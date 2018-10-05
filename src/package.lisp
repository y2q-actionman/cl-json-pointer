(in-package :cl-user)

(defpackage #:cl-json-pointer
  (:use :cl)
  (:use #:alexandria)
  (:import-from #:closer-mop
		#:class-slots
		#:slot-definition-name
		#:slot-boundp-using-class
		#:slot-value-using-class
		#:slot-makunbound-using-class)
  (:export
   #:json-pointer-error
   #:*json-object-type*
   #:parse-json-pointer
   #:get-by-json-pointer
   #:exists-p-by-json-pointer
   #:set-by-json-pointer
   #:add-by-json-pointer
   #:delete-by-json-pointer
   #:remove-by-json-pointer
   #+ (or) #:update-by-json-pointer))
