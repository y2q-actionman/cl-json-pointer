;;; Test codes by:
;;; https://github.com/janl/node-jsonpointer

(in-package :cl-json-pointer/test)
(in-readtable cjp-test-syntax)

;;; Test cases in top page.

(define-constant +example-test1-1+
    #{ "foo": 1, "bar": { "baz": 2}, "qux": [3, 4, 5]}
    :test 'equal)

(defmacro assert-condition (&body body)
  (with-gensyms (ret condition)
    `(multiple-value-bind (,ret ,condition)
	 (ignore-errors (progn ,@body))
       (assert (and (null ,ret) ,condition)))))

(defun test1-1 (&aux (obj (read-json-string +example-test1-1+)))
  (assert (equal (get-by-json-pointer obj "/foo") 1))
  (assert (equal (get-by-json-pointer obj "/bar/baz") 2))
  (assert (equal (get-by-json-pointer obj "/qux/0") 3))
  (assert (equal (get-by-json-pointer obj "/qux/1") 4))
  (assert (equal (get-by-json-pointer obj "/qux/2") 5))
  (assert (not (get-by-json-pointer obj "/quo")))

  (set-by-json-pointer obj "/foo" 6)
  (assert (equal (get-by-json-pointer obj "/foo") 6))
  (setf (get-by-json-pointer obj "/foo") 7)
  (assert (equal (get-by-json-pointer obj "/foo") 7))

  (set-by-json-pointer obj "/qux/-" 6)
  (assert (equalp (get-by-json-pointer obj "/qux")
		  #(3 4 5 6)))
  (setf (get-by-json-pointer obj "/qux/-") 99)
  (assert (equalp (get-by-json-pointer obj "/qux")
		  #(3 4 5 6 99)))

  (let ((pointer (parse-json-pointer "/foo")))
    (assert (equal (get-by-json-pointer obj pointer) 7))
    (setf (get-by-json-pointer obj pointer) 999)
    (assert (equal (get-by-json-pointer obj pointer) 999)))

  t)
