(in-package :cl-json-pointer)

(defmethod traverse-by-reference-token
    ((kind (eql :com.gigamonkeys.json)) (obj list) (rtoken string) set-method next-setter)
  (list-try-traverse (list* :plist *traverse-object-like-kinds*)
		     obj rtoken set-method next-setter))

(defmethod traverse-by-reference-token
    ((kind (eql :com.gigamonkeys.json)) (obj null) rtoken set-method next-setter)
  (let ((*traverse-nil-set-to-last-method* :list)
  	(*traverse-nil-set-to-index-method* :list)
  	(*traverse-nil-set-to-name-method* :plist))
  (call-next-method)))
