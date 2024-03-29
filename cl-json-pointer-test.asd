;;; Some libs are not in Quicklisp.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-system :com.inuoe.jzon nil)
    (pushnew :cl-json-pointer/test/com.inuoe.jzon *features*))
  (when (find-system :json-lib nil)
    (pushnew :cl-json-pointer/test/json-lib *features*)))

(defsystem #:cl-json-pointer/test
  :description "Tests for cl-json-pointer."
  :licence "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (#:cl-json-pointer
	       #:cl-json-pointer/synonyms
	       ;; test libs
	       #:named-readtables #:1am
	       ;; All Json libs and platform supports (alphabetical order)
               (:feature :cl-json-pointer/boost-json-support #:cl-json-pointer/boost-json-support) ; not in Quicklisp.
	       #:cl-json
	       #:com.gigamonkeys.json
               (:feature :cl-json-pointer/test/com.inuoe.jzon #:com.inuoe.jzon) ; not in Quicklisp.
	       #:jonathan ; I surprised this lib has 8 dependencies.
               (:feature :cl-json-pointer/test/json-lib #:json-lib) ; not in Quicklisp.
	       #:json-streams
	       #:jsown
	       #:shasht
	       #:cl-json-pointer/st-json-support ; st-json
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
	     (:file "test-top-page")
             (:file "test-trivial-json-codec"))))
  :perform (prepare-op :before (o c)
             (set (find-symbol* :*tests* :1am) '() ))
  :perform (test-op (o s) (symbol-call '#:cl-json-pointer/test '#:run)))
