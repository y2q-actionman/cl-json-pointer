(in-package :cl-json-pointer)

;;; json-lib does not use lists for objects nor arrays by default.
;;; I don't need to customize list traversal methods.

(pushnew :json-lib *cl-json-pointer-supported-json-flavors*)
