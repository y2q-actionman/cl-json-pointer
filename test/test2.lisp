;;; Test codes by:
;;; https://github.com/alexeykuzmin/jsonpointer.js

(in-package :cl-json-pointer/test)
(in-readtable cjp-test-syntax)

(define-constant +test2-array+
#[
  { "foo": "bar", "baz": [1, 2, 3] } ,
  { "foo": "foobar"}
]
  :test 'equal)

(defun test2-array (&aux (obj (read-json-string +test2-array+)))
  (assert (equal (get-by-json-pointer obj "/0/foo") "bar"))
  (assert (equal (get-by-json-pointer obj "/0/baz/1") 2))
  (assert (equalp (get-by-json-pointer obj "/1")
		  (read-json-string #{ "foo": "foobar" })))
  t)


(define-constant +test2-undefined+
  #{
  "foo": "bar",
  "baz": [1, 2, 3]
  }
  :test 'equal)

(defun test2-undefined (&aux (obj (read-json-string +test2-undefined+)))
  (assert (not (exists-p-by-json-pointer obj "/oof")))
  (assert (not (exists-p-by-json-pointer obj "/baz/4")))
  ;; (assert (not (exists-p-by-json-pointer obj "/foo/bar")))
  ;; (assert (not (exists-p-by-json-pointer obj "/foo/bar/baz")))
  t)


(define-constant +test2-bad-pointer+
  #{
   "foo": "bar",
   "baz": [1, 2, 3],
   "-": "valid"
  }
  :test 'equal)

(defun test2-bad-pointer (&aux (obj (read-json-string +test2-bad-pointer+)))
  (assert-condition (parse-json-pointer "a"))
  (assert-condition (get-by-json-pointer obj "/baz/01")) ; My impl does not report error at parsing.
  ;; (assert-condition (get-by-json-pointer obj "/baz/-")) ; this case is curious. I think this is valid.
  (assert-condition (parse-json-pointer "-"))
  t)


(defun test2-valid-pointer ()
  (assert (null (parse-json-pointer ""))) ; In my impl, this returns nil.
  (assert (parse-json-pointer "/"))
  (assert (parse-json-pointer "//"))
  (assert (parse-json-pointer "/a"))
  (assert (parse-json-pointer "/0"))
  (assert (parse-json-pointer "/10"))
  (assert (parse-json-pointer "/a/0"))
  (assert (parse-json-pointer "/1/a"))
  (assert (parse-json-pointer "/-"))
  t)


(defun test2-run ()
  (and (test2-array)
       (test2-undefined)
       (test2-bad-pointer)
       (test2-valid-pointer)))
