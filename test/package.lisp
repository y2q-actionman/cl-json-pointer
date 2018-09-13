(in-package :cl-user)

(defpackage :cl-json-pointer/test
  (:use :cl :cl-json-pointer)
  (:import-from #:alexandria
		#:if-let
		#:when-let)
  (:export
   #:run))
