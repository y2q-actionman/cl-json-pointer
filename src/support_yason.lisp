(in-package :cl-json-pointer)

(defmethod traverse-by-reference-token
    ((flavor (eql :yason)) (obj list) (rtoken string) set-method next-setter)
  ;; An optimization -- don't consider plist.
  (list-try-traverse '(:alist) flavor obj rtoken set-method next-setter))

(pushnew :yason *cl-json-pointer-supported-json-flavors*)
