(in-package :cl-json-pointer)

(defmethod traverse-by-reference-token ((obj st-json:jso) obj-type rtoken set-method next-setter)
  (multiple-value-bind (val exists)
      (st-json:getjso rtoken obj)
    (values val exists
	    (ecase set-method
	      ((nil) nil)
	      ((:update :add)
	       (chained-setter-lambda (x) (next-setter obj)
		 (setf (st-json:getjso rtoken obj) x)))
	      ((:delete :remove)
	       (chained-setter-lambda () (next-setter obj)
		 ;; FIXME: This code uses internal symbol..
		 (let* ((internal-alist (st-json::jso-alist obj))
			(alist-setter
			 (nth-value 2 (traverse-list-by-reference-token
				       :alist internal-alist rtoken set-method
				       (lambda (x)
					 (setf (st-json::jso-alist obj) x))))))
		   (funcall alist-setter))))))))
