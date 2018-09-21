(in-package :cl-user)

(asdf:defsystem #:cl-json-pointer
  :licence "MIT"
  :depends-on (#:alexandria #:closer-mop)
  ;; asdf3 says `:weakly-depends-on' is deprecated..
  ;; :weakly-depends-on (#:cl-json)
  :serial t
  :components
  ((:module "src"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "util")
	     (:file "condition")
	     (:file "parser")
	     (:file "traversal")
	     (:file "interface"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-json-pointer/test))))

(asdf:defsystem #:cl-json-pointer/synonyms
  :licence "MIT"
  :depends-on (#:cl-json-pointer)
  :serial t
  :components
  ((:module "synonyms"
	    :components
	    ((:file "synonyms")))))

(asdf:defsystem #:cl-json-pointer/test
  :licence "MIT"
  :depends-on (#:cl-json-pointer #:cl-json-pointer/synonyms
	       #:named-readtables
	       #:cl-json)		; fixme
  :serial t
  :components
  ((:module "test"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "util")
	     (:file "cl-json")
	     (:file "test0")
	     (:file "test1")
	     (:file "test2")
	     (:file "test"))))
  :perform (asdf:test-op (o s) (uiop:symbol-call '#:cl-json-pointer/test '#:run)))
