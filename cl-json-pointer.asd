(in-package :cl-user)

;;; The core package. (includes cl-json and yason)
(asdf:defsystem #:cl-json-pointer/core
  :licence "MIT"
  :depends-on (#:alexandria #:closer-mop)
  :components
  ((:module "src"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "util")
	     (:file "condition")
	     (:file "parser")
	     (:file "traversal")
	     (:file "interface")))))

;;; st-json support
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :st-json) ; If ST-JSON has been loaded, I load specific codes.
    (pushnew :cl-json-pointer/st-json-support *features*)))

(asdf:defsystem #:cl-json-pointer/st-json-support
  :licence "MIT"
  :depends-on (#:cl-json-pointer/core #:st-json)
  :components
  ((:module "src" :components ((:file "st-json-support")))))

;;; jsown support
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :jsown)		; If Jsown has been loaded...
    (pushnew :cl-json-pointer/jsown-support *features*)))

(asdf:defsystem #:cl-json-pointer/jsown-support
  :licence "MIT"
  :depends-on (#:cl-json-pointer/core #:jsown)
  :components
  ((:module "src" :components ((:file "jsown-support")))))

;;; The main defsystem.
(asdf:defsystem #:cl-json-pointer
  :licence "MIT"
  :depends-on (#:cl-json-pointer/core
	       (:feature :cl-json-pointer/st-json-support
			 #:cl-json-pointer/st-json-support)
	       (:feature :cl-json-pointer/jsown-support
			 #:cl-json-pointer/jsown-support))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-json-pointer/test))))

;;; For convenience.
(asdf:defsystem #:cl-json-pointer/synonyms
  :licence "MIT"
  :depends-on (#:cl-json-pointer)
  :components
  ((:module "synonyms" :components ((:file "synonyms")))))

;;; Testing
(asdf:defsystem #:cl-json-pointer/test
  :licence "MIT"
  :depends-on (#:cl-json-pointer
	       #:cl-json-pointer/synonyms
	       ;; enables all platform supports
	       #:cl-json-pointer/st-json-support #:cl-json-pointer/jsown-support
	       ;; test libs
	       #:named-readtables #:1am
	       ;; json libs
	       #:cl-json #:st-json #:yason #:jsown
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
