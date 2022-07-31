;;; Some libs are not in Quicklisp.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :com.inuoe.jzon)
    (pushnew :cl-json-pointer/test/com.inuoe.jzon *features*)))

(defsystem #:cl-json-pointer/test
  :description "Tests for cl-json-pointer."
  :licence "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:cl-json-pointer
	       #:cl-json-pointer/synonyms
	       ;; test libs
	       #:named-readtables #:1am
	       ;; All Json libs and platform supports (alphabetical order)
	       #:cl-json
	       #:cl-json-pointer/st-json-support
	       #:com.gigamonkeys.json
               (:feature :cl-json-pointer/test/com.inuoe.jzon #:com.inuoe.jzon)
	       #:jonathan ; I surprised this lib has 8 dependencies.
	       #:json-streams
	       #:jsown
	       #:shasht
	       #:trivial-json-codec
	       #:yason
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
	     (:file "test-utils")
	     (:file "test0")
	     (:file "test1")
	     (:file "test2")
	     (:file "test3")
	     (:file "test4")
	     (:file "test-top-page"))))
  :perform (prepare-op :before (o c)
             (set (find-symbol* :*tests* :1am) '() ))
  :perform (test-op (o s) (symbol-call '#:cl-json-pointer/test '#:run)))
