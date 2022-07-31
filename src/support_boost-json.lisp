(in-package :cl-json-pointer)

;; boost-json has its own CLOS class to handle JSON objects.

(defmethod traverse-by-reference-token (flavor (obj boost-json:json-object)
                                        rtoken set-method next-setter)
  (declare (ignorable flavor))
  (multiple-value-bind (val exists)
      (boost-json:json-getf obj rtoken)
    (values val exists
	    (ecase set-method
	      ((nil) nil)
	      ((:update :add)
	       (chained-setter-lambda (x) (next-setter obj)
		 (boost-json:json-setf obj rtoken x)))
	      ((:delete :remove)
	       (chained-setter-lambda () (next-setter obj)
                 ;; This code potion is almost same with ST-JSON support.
		 (let* ((internal-alist (boost-json:json-object-members obj))
			(alist-setter
			 (nth-value 2 (traverse-by-reference-token
				       :alist internal-alist rtoken set-method
				       (lambda (updated-alist)
                                         (setf (boost-json:json-object-members obj) updated-alist))))))
		   (funcall alist-setter))))))))

(pushnew :st-json *cl-json-pointer-supported-json-flavors*)
