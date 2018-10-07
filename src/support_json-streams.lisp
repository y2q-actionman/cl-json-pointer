(in-package :cl-json-pointer)

(defmethod traverse-by-reference-token ((kind (eql :json-streams)) (obj list)
					(rtoken string) set-method next-setter)
  (case (car obj)
    (:object
     (traverse-by-reference-token :alist (cdr obj)
				   rtoken set-method
				   (chained-setter-lambda (x) (next-setter obj)
				     (setf (cdr obj) x))))
    (:array
     (traverse-by-reference-token :list (cdr obj)
				   rtoken set-method
				   (chained-setter-lambda (x) (next-setter obj)
				     (setf (cdr obj) x))))
    (t
     (call-next-method))))

(defmethod traverse-by-reference-token ((kind (eql :json-streams)) (obj null)
					rtoken set-method next-setter)
  (cond ((null set-method)
	 (values nil nil nil))
	((member set-method '(:delete :remove))
	 (values nil nil
		 (thunk-lambda
		   (bad-deleter-error obj rtoken))))
	((read-reference-token-as-index rtoken nil)
	 (values nil nil
		 (lambda (x)
		   (let* ((tmp nil)
			  (internal-setter
			   (nth-value 2 (traverse-by-reference-token
					 :list nil rtoken set-method
					 (lambda (x)
					   (setf tmp x))))))
		     (funcall internal-setter x) ; `tmp' gains the newly created list.
		     (push :array tmp)
		     (funcall next-setter tmp)))))
	(t
	 (values nil nil
		 (chained-setter-lambda (x) (next-setter)
		   `(:object (,rtoken . ,x)))))))
