(in-package :cl-user)

(defpackage :cl-json-pointer/test
  (:use :cl :cl-json-pointer)
  (:use :alexandria :named-readtables)
  (:export
   #:run))
