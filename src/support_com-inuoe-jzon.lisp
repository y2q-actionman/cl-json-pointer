(in-package :cl-json-pointer)

;;; com.inuoe.jzon does not use lists for objects nor arrays. So I
;;; don't need to customize list traversal methods.

(pushnew :com.inuoe.jzon *cl-json-pointer-supported-json-flavors*)
