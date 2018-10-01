(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :st-json)
    (pushnew :cl-json-pointer/st-json-support *features*)))

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
	     (:file "interface")
	     (:file "st-json-support" :if-feature :cl-json-pointer/st-json-support))))
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
	       #:named-readtables #:1am
	       ;; json libs
	       #:cl-json #:st-json #:yason
	       ;; TODO
	       ;;  #:jsown
	       ;; #:jonathan ; I surprised this lib has 8 dependencies.
	       ;; #:json-streams #:com.gigamonkeys.json
	       ;; Not supported
	       ;; #:define-json-expander #:json-mop
	       )
  :serial t
  :components
  ((:module "test"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "util")
	     ;; 
	     (:file "reader")
	     ;; 
	     (:file "test0")
	     (:file "test1")
	     (:file "test2")
	     (:file "test3")
	     (:file "test4")
	     (:file "test-top-page"))))
  :perform (asdf:prepare-op :before (o c)
             (set (uiop:find-symbol* :*tests* :1am) '() ))
  :perform (asdf:test-op (o s) (uiop:symbol-call '#:cl-json-pointer/test '#:run)))
