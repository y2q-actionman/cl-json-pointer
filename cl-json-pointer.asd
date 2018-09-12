(in-package :cl-user)

(asdf:defsystem #:cl-json-pointer
  :licence "MIT"
  :depends-on (#:alexandria #:closer-mop)
  ;; asdf 3 says `:weakly-depends-on' is deprecated..
  :weakly-depends-on (#:cl-json)
  :serial t
  :components ((:file "package")
	       (:file "condition")
               (:file "parser")
               (:file "traversal")))

(asdf:defsystem #:cl-json-pointer/test
  :licence "MIT"
  :depends-on (#:cl-json-pointer)
  :serial t
  :components
  ((:module "test"
	    :serial t
	    :components
	    ((:file "cl-json.lisp")
	     (:file "test.lisp")))))

