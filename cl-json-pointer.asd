;;; The core package. (includes cl-json, yason, etc.)
(defsystem #:cl-json-pointer/core
  :description "cl-json-pointer core files."
  :licence "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:alexandria #:access)
  :components
  ((:module "src"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "util")
	     (:file "condition")
	     (:file "parser")
	     (:file "traversal")
	     (:file "interface")
	     (:file "support_library")
	     (:file "support_cl-json")))))

;;; Some library support. 
(defsystem #:cl-json-pointer/st-json-support
  :description "cl-json-pointer st-json support."
  :licence "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:cl-json-pointer/core #:st-json)
  :components ((:module "src" :components ((:file "support_st-json")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :st-json)
    (pushnew :cl-json-pointer/st-json-support *features*)))

;;; The main defsystem.
(defsystem #:cl-json-pointer
  :description "A JSON Pointer (RFC6901) implementation for Common Lisp."
  :licence "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:cl-json-pointer/core
	       (:feature :cl-json-pointer/st-json-support
			 #:cl-json-pointer/st-json-support))
  :in-order-to ((test-op (test-op #:cl-json-pointer/test))))

;;; For convenience.
(defsystem #:cl-json-pointer/synonyms
  :description "Extra functions for cl-json-pointer."
  :licence "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:cl-json-pointer)
  :components
  ((:module "synonyms" :components ((:file "synonyms")))))
