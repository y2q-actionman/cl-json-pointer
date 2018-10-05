(in-package :cl-json-pointer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :jsown *traverse-object-like-kinds*))

(defmethod traverse-by-reference-token ((kind (eql :jsown)) (jsown-obj list)
					rtoken set-method next-setter)
  (if (eq (car jsown-obj) :OBJ)
      (traverse-by-reference-token :alist (cdr jsown-obj)
				   rtoken set-method
				   (chained-setter-lambda (x) (next-setter jsown-obj)
				     (setf (cdr jsown-obj) x)))
      ;; TODO: FIXME: I think this path is redundant..
      (values nil nil
	      (ecase set-method
		((nil) nil)
		((:delete :remove)
		 (thunk-lambda
		   (bad-deleter-error jsown-obj rtoken)))
		((:update :add)
		 (chained-setter-lambda (x) (next-setter)
		   `(:OBJ (,rtoken . ,x))))))))
