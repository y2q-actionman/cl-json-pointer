;;; Test codes by:
;;; https://github.com/manuelstofer/json-pointer/blob/master/test/test.js

(in-package :cl-json-pointer/test)
(in-readtable cjp-test-syntax)

(1am:test test3-get
  (let ((obj (make-instance 'standard-object)))
    (1am:is (eq obj (cljsp:get obj "")))))
