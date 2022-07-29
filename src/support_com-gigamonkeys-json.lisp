(in-package :cl-json-pointer)

(defmethod traverse-by-reference-token
    ((flavor (eql :com.gigamonkeys.json)) (obj list) (rtoken string) set-method next-setter)
  ;; An optimization -- don't consider alist.
  (list-try-traverse '(:plist) flavor obj rtoken set-method next-setter))

(defmethod traverse-by-reference-token
    ((flavor (eql :com.gigamonkeys.json)) (obj null) rtoken set-method next-setter)
  (declare (ignorable rtoken set-method next-setter))
  (let ((*traverse-nil-set-to-name-method* :plist))
    (call-next-method)))

(pushnew :com.gigamonkeys.json *cl-json-pointer-supported-json-flavors*)
