(in-package :cl-json-pointer)

(define-constant +identifier-name-to-key-name+
  (symbol-name '*identifier-name-to-key*)
  :test #'equal)

(define-constant +json-identifier-name-to-lisp-name+
  (symbol-name '*json-identifier-name-to-lisp*)
  :test #'equal)

(defmethod intern-object-key ((flavor (eql :cl-json)) rtoken)
  ;; Do Like:
  ;; (funcall cl-json:*identifier-name-to-key*
  ;; 	   (funcall cl-json:*json-identifier-name-to-lisp* rtoken)))
  (when-let* ((cl-json-package (find-package :cl-json))
	      (i-n-t-k-symbol (find-symbol +identifier-name-to-key-name+
					   cl-json-package))
	      (j-i-n-t-l-symbol (find-symbol +json-identifier-name-to-lisp-name+
					     cl-json-package)))
    (return-from intern-object-key
      (funcall (symbol-value i-n-t-k-symbol)
	       (funcall (symbol-value j-i-n-t-l-symbol) rtoken))))
  (call-next-method))

(pushnew :cl-json *cl-json-pointer-supported-json-flavors*)
