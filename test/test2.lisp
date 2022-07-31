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

(defmethod json-object-equal-p (obj1 obj2)
  (equalp obj1 obj2))

#+cl-json-pointer/boost-json-support
(defmethod json-object-equal-p ((obj1 boost-json:json-object) (obj2 boost-json:json-object))
  "Currently, boost-json only has its own class for JSON object.
 (st-json has its own type, but it is a structure, which `cl:equalp' works."
  (equalp (boost-json:json-object-members obj1)
          (boost-json:json-object-members obj2)))

(1am:test test2-array
  (let ((obj (read-json-string +test2-array+)))
    (1am:is (equal (get-by-json-pointer obj "/0/foo") "bar"))
    (1am:is (equal (get-by-json-pointer obj "/0/baz/1") 2))
    (1am:is (json-object-equal-p (get-by-json-pointer obj "/1")
		                 (read-json-string #{ "foo": "foobar" })))))


(define-constant +test2-undefined+
  #{
  "foo": "bar",
  "baz": [1, 2, 3]
  }
  :test 'equal)

(1am:test test2-undefined
  (let ((obj (read-json-string +test2-undefined+)))
    (1am:is (not (exists-p-by-json-pointer obj "/oof")))
    (1am:is (not (exists-p-by-json-pointer obj "/baz/4")))
    (1am:is (not (exists-p-by-json-pointer obj "/foo/bar")))
    (1am:is (not (exists-p-by-json-pointer obj "/foo/bar/baz")))))


(define-constant +test2-bad-pointer+
  #{
   "foo": "bar",
   "baz": [1, 2, 3],
   "-": "valid"
  }
  :test 'equal)

(1am:test test2-bad-pointer
  (let ((obj (read-json-string +test2-bad-pointer+)))
    (1am:signals cl-json-pointer:json-pointer-error
      (parse-json-pointer "a"))
    (1am:signals cl-json-pointer:json-pointer-error
      (get-by-json-pointer obj "/baz/01")) ; My impl does not report error at parsing.
    ;; this case is curious. I think this is valid.
    #+()
    (1am:signals cl-json-pointer:json-pointer-error
      (get-by-json-pointer obj "/baz/-"))
    (1am:signals cl-json-pointer:json-pointer-error
      (parse-json-pointer "-"))))


(1am:test test2-valid-pointer ()
  (1am:is (null (parse-json-pointer ""))) ; In my impl, this returns nil.
  (1am:is (parse-json-pointer "/"))
  (1am:is (parse-json-pointer "//"))
  (1am:is (parse-json-pointer "/a"))
  (1am:is (parse-json-pointer "/0"))
  (1am:is (parse-json-pointer "/10"))
  (1am:is (parse-json-pointer "/a/0"))
  (1am:is (parse-json-pointer "/1/a"))
  (1am:is (parse-json-pointer "/-")))
