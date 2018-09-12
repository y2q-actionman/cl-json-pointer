(in-package :cl-user)

(defpackage cl-json-pointer
  (:use :cl :cl-json)
  (:import-from :alexandria
		#:when-let
		#:named-lambda)
  (:import-from :closer-mop
		#:class-slots
		#:slot-definition-name
		#:slot-boundp-using-class
		#:slot-value-using-class)
  (:export
   ))
