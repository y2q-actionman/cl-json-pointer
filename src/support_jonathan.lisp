(in-package :cl-json-pointer)

;;; TODO: support :AS flavors
(defmethod traverse-by-reference-token
    ((flavor (eql :jonathan)) (obj null) rtoken set-method next-setter)
  (declare (ignorable rtoken set-method next-setter))
  (let ((*traverse-nil-set-to-name-method* :plist)) ; default plist one.
    (call-next-method)))

(defmethod intern-object-key ((flavor (eql :jonathan)) rtoken)
  (intern rtoken (find-package :keyword)))

(pushnew :jonathan *cl-json-pointer-supported-json-flavors*)
