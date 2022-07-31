(in-package :cl-json-pointer)

;;; `trivial-json-codec:deserialize' works well. See test/test-trivial-json-codec.lisp file.
;;; `trivial-json-codec:deserialize-raw' requires special handlings.

(defmethod intern-object-key ((flavor (eql :trivial-json-codec)) (rtoken string))
  "It seems trivial-json-codec ignores escape characters (backslashes).

  For example, the result of
  (trivial-json-codec:deserialize-raw cl-json-pointer/test::+test1-example+)
  contains (:I\\\\J 5). I think this should be (:I\\J 5)

But, this method treats it in that way."
  (loop with tmpstr = (make-array (length rtoken) :element-type 'character
                                                  :adjustable t :fill-pointer 0)
        for c across rtoken
        if (or (char= c #\\) (char= c #\"))
          do (vector-push-extend #\\ tmpstr)
             (vector-push-extend c tmpstr)
        else
          do (vector-push-extend (char-upcase c) tmpstr)
        finally
           (return (intern tmpstr :keyword))))

(defmethod traverse-by-reference-token ((flavor (eql :trivial-json-codec-alist)) (alist list)
                                        rtoken set-method next-setter)
  "The result of `trivial-json-codec:deserialize-raw' looks like an
alist, but its cdr is wrapped by a cons:

   (trivial-json-codec:deserialize-raw \"{ \\\"a\\\": 1, \\\"b\\\": 2}\" )
   ; => ((:A 1) (:B 2))

This method treats them specially."
  (flet ((add-to-head (x)
	   (acons rtoken (list x) alist))) ; assumes RTOKEN is interned.
    (if-let ((entry (assoc rtoken alist :test #'compare-string-by-readtable-case)))
      (values (cadr entry) entry
	      (ecase set-method
		((nil) nil)
		(:update
		 (chained-setter-lambda (x) (next-setter alist)
		   (setf (cadr entry) x)))
		(:add
		 (chained-setter-lambda (x) (next-setter)
		   (add-to-head x)))
		(:delete
		 (chained-setter-lambda () (next-setter)
		   (delete entry alist)))
		(:remove
		 (chained-setter-lambda () (next-setter)
		   (remove entry alist)))))
      (values nil nil
	      (ecase set-method
		((nil) nil)
		((:add :update)
		 (chained-setter-lambda (x) (next-setter)
		   (add-to-head x)))
		((:delete :remove)
		 (thunk-lambda
		   (bad-deleter-error alist rtoken))))))))

(defmethod traverse-by-reference-token
    ((flavor (eql :trivial-json-codec)) (obj list) (rtoken string) set-method next-setter)
  (list-try-traverse '(:trivial-json-codec-alist)
                     flavor obj rtoken set-method next-setter))

(defmethod traverse-by-reference-token :around
    ((flavor (eql :trivial-json-codec)) (obj null) rtoken set-method next-setter)
  (declare (ignorable rtoken set-method next-setter))
  (let ((*traverse-nil-set-to-last-method* :array))
    (call-next-method)))

(pushnew :trivial-json-codec *cl-json-pointer-supported-json-flavors*)
