(in-package :cl-user)

(asdf:defsystem #:cl-json-pointer
  :licence "MIT"
  :depends-on (#:alexandria #:cl-json)	; fixme
  :serial t
  :components ((:file "package")
               (:file "main")
	       (:file "cl-json")))

#+ignore 
(asdf:defsystem #:cl-json-pointer/test
  :licence "MIT"
  :depends-on (#:cl-json-pointer)
  :serial t
  :components ())

