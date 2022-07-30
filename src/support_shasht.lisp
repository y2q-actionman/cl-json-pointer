(in-package :cl-json-pointer)

;;; shasht does not use lists for objects nor arrays by default.
;;;
;;; And shasht can be use lists by changing the mappings.  I think the
;;; default traversal methods in cl-json-pointer can treat them.

(pushnew :shasht *cl-json-pointer-supported-json-flavors*)
