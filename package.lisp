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
   #:json-pointer-get
   #:json-pointer-set
   ))
   


;; TODO: node-jsonpointer interface
;; - https://github.com/janl/node-jsonpointer
;; - https://github.com/alexeykuzmin/jsonpointer.js
;; (shadow :get :set)
;; (export :get :set)
;;
;; TODO: json query
;; - https://github.com/sagold/json-query
