(in-package :cl-user)

(asdf:defsystem #:cl-json-pointer
  :licence "MIT"
  :depends-on (#:alexandria #:closer-mop)
  ;; asdf3 says `:weakly-depends-on' is deprecated..
  ;; :weakly-depends-on (#:cl-json)
  :serial t
  :components ((:file "package")
	       (:file "condition")
               (:file "parser")
               (:file "traversal")
	       (:file "interface")))

(asdf:defsystem #:cl-json-pointer/test
  :licence "MIT"
  :depends-on (#:cl-json-pointer #:named-readtables
	       #:cl-json)		; fixme
  :serial t
  :components
  ((:module "test"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "reader")
	     (:file "cl-json")
	     (:file "test1")
	     (:file "test")))))

