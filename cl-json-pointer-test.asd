(in-package :cl-user)

(asdf:defsystem #:cl-json-pointer/test
  :description "Tests for cl-json-pointer."
  :licence "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:cl-json-pointer
	       #:cl-json-pointer/synonyms
	       ;; test libs
	       #:named-readtables #:1am
	       ;; All Json libs and platform supports
	       #:cl-json
	       #:cl-json-pointer/st-json-support
	       #:yason
	       #:jsown
	       #:jonathan ; I surprised this lib has 8 dependencies.
	       #:json-streams
	       #:com.gigamonkeys.json
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
