(in-package :cl-json-pointer)

(defmethod traverse-by-reference-token ((flavor (eql :json-streams)) (obj list)
					(rtoken string) set-method next-setter)
  (case (car obj)
    (:object
     (traverse-by-reference-token :alist (cdr obj)
				  (intern-object-key flavor rtoken)
				  set-method
				  (chained-setter-lambda (x) (next-setter obj)
				    (setf (cdr obj) x))))
    (:array
     (traverse-by-reference-token :list (cdr obj)
				  rtoken set-method
				  (chained-setter-lambda (x) (next-setter obj)
				    (setf (cdr obj) x))))
    (t
     (call-next-method))))

(defmethod traverse-by-reference-token ((flavor (eql :json-streams)) (obj null)
					rtoken set-method next-setter)
  (cond ((null set-method)
	 (values nil nil nil))
	((member set-method '(:delete :remove))
	 (values nil nil
		 (thunk-lambda
		   (bad-deleter-error obj rtoken))))
	(t
	 (if-let ((index (read-reference-token-as-index rtoken nil)))
	   (values nil nil
		   (lambda (x)
		     (let* ((tmp nil)
			    (internal-setter
			     (nth-value 2 (traverse-by-reference-token
					   :list nil index set-method
					   (lambda (x)
					     (setf tmp x))))))
		       (funcall internal-setter x) ; TMP gains the newly created list.
		       (funcall next-setter (list* :array tmp)))))
	   (values nil nil
		   (chained-setter-lambda (x) (next-setter)
		     `(:object (,(intern-object-key flavor rtoken) . ,x))))))))

(pushnew :json-streams *cl-json-pointer-supported-json-flavors*)
