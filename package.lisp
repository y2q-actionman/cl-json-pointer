(in-package :cl-user)

(defpackage cl-json-pointer
  (:use :cl :cl-json))

;; TODO: node-jsonpointer interface
;; - https://github.com/janl/node-jsonpointer
;; - https://github.com/alexeykuzmin/jsonpointer.js
;; (shadow :get :set)
;; (export :get :set)
;;
;; TODO: json query
;; - https://github.com/sagold/json-query
