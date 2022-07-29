(in-package :cl-json-pointer)

(defmethod traverse-by-reference-token ((flavor (eql :jsown)) (obj list)
					(rtoken string) set-method next-setter)
  (if (eq (car obj) :OBJ)
      (traverse-by-reference-token :alist (cdr obj)
				   (intern-object-key flavor rtoken)
				   set-method
				   (chained-setter-lambda (x) (next-setter obj)
				     (setf (cdr obj) x)))
      (call-next-method)))

(defmethod traverse-by-reference-token ((flavor (eql :jsown)) (obj null)
					rtoken set-method next-setter)
  (cond ((null set-method)
	 (values nil nil nil))
	((read-reference-token-as-index rtoken nil)
	 (call-next-method))
	(t
	 (values nil nil
		 (ecase set-method
		   ((:delete :remove)
		    (thunk-lambda
		      (bad-deleter-error obj rtoken)))
		   ((:update :add)
		    (chained-setter-lambda (x) (next-setter)
		      `(:OBJ (,(intern-object-key flavor rtoken) . ,x)))))))))

(pushnew :jsown *cl-json-pointer-supported-json-flavors*)
