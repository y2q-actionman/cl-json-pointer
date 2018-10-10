(in-package :cl-json-pointer)

(defmethod intern-object-key ((flavor (eql :cl-json)) rtoken)
  (funcall cl-json:*identifier-name-to-key*
	   (funcall cl-json:*json-identifier-name-to-lisp* rtoken)))

(pushnew :cl-json *cl-json-pointer-supported-json-flavors*)
