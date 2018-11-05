(in-package :cl-json-pointer)

(defvar *cl-json-pointer-supported-json-flavors* nil)

;;; yason
(defmethod traverse-by-reference-token
    ((kind (eql :yason)) (obj list) (rtoken string) set-method next-setter)
  ;; An optimization -- don't consider plist.
  (list-try-traverse '(:alist) obj rtoken set-method next-setter))

(pushnew :yason *cl-json-pointer-supported-json-flavors*)

;;; jsown
(defmethod traverse-by-reference-token ((kind (eql :jsown)) (obj list)
					(rtoken string) set-method next-setter)
  (if (eq (car obj) :OBJ)
      (traverse-by-reference-token :alist (cdr obj)
				   rtoken set-method
				   (chained-setter-lambda (x) (next-setter obj)
				     (setf (cdr obj) x)))
      (call-next-method)))

(defmethod traverse-by-reference-token ((kind (eql :jsown)) (obj null)
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
		      `(:OBJ (,rtoken . ,x)))))))))

(pushnew :jsown *cl-json-pointer-supported-json-flavors*)

;;; jonathan
;;; TODO: support `:as' flavors
(defmethod traverse-by-reference-token
    ((kind (eql :jonathan)) (obj null) rtoken set-method next-setter)
  (declare (ignorable rtoken set-method next-setter))
  (let ((*traverse-nil-set-to-name-method* :plist)) ; default plist one.
    (call-next-method)))

(defmethod intern-object-key ((flavor (eql :jonathan)) rtoken)
  (intern rtoken (find-package :keyword)))

(pushnew :jonathan *cl-json-pointer-supported-json-flavors*)

;;; json-streams
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
		       (funcall internal-setter x) ; `tmp' gains the newly created list.
		       (funcall next-setter (list* :array tmp)))))
	   (values nil nil
		   (chained-setter-lambda (x) (next-setter)
		     `(:object (,rtoken . ,x))))))))

(pushnew :json-streams *cl-json-pointer-supported-json-flavors*)

;;; com.gigamonkeys.json
(defmethod traverse-by-reference-token
    ((kind (eql :com.gigamonkeys.json)) (obj list) (rtoken string) set-method next-setter)
  ;; An optimization -- don't consider alist.
  (list-try-traverse '(:plist) obj rtoken set-method next-setter))

(defmethod traverse-by-reference-token
    ((kind (eql :com.gigamonkeys.json)) (obj null) rtoken set-method next-setter)
  (declare (ignorable rtoken set-method next-setter))
  (let ((*traverse-nil-set-to-name-method* :plist))
    (call-next-method)))

(pushnew :com.gigamonkeys.json *cl-json-pointer-supported-json-flavors*)
