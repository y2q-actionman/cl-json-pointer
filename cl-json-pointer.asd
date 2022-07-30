;;; The core package. (includes cl-json, yason, etc.)
(defsystem #:cl-json-pointer/core
  :description "cl-json-pointer core files."
  :licence "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:alexandria #:closer-mop)
  :components
  ((:module "src"
	    :components
	    ((:file "package")
	     (:file "util" :depends-on ("package"))
	     (:file "condition" :depends-on ("package"))
	     (:file "parser" :depends-on ("condition"))
	     (:file "traversal" :depends-on ("util" "condition" "parser"))
	     (:file "interface" :depends-on ("traversal"))
	     (:file "support" :depends-on ("traversal"))
             ;; alphabetical order
	     (:file "support_cl-json" :depends-on ("support"))
             (:file "support_com-gigamonkeys-json" :depends-on ("support"))
             (:file "support_com-inuoe-jzon" :depends-on ("support"))
             (:file "support_jonathan" :depends-on ("support"))
             (:file "support_json-streams" :depends-on ("support"))
             (:file "support_jsown" :depends-on ("support"))
             (:file "support_shasht" :depends-on ("support"))
             (:file "support_yason" :depends-on ("support"))
             ;; Supporting st-json has its own defsystem because it
             ;; requires the real dependency. See below.
             ))))

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
