(in-package :cl-json-pointer/test)

;;; my top page
(defparameter *rfc6901-example*
  "{
   \"foo\": [\"bar\", \"baz\"],
   \"\": 0,
   \"a/b\": 1,
   \"c%d\": 2,
   \"e^f\": 3,
   \"g|h\": 4,
   \"i\\\\j\": 5,
   \"k\\\"l\": 6,
   \" \": 7,
   \"m~n\": 8
   }")

(defun test-top-page ()
  (let ((obj (cl-json:decode-json-from-string *rfc6901-example*)))
    (assert (eql obj (get-by-json-pointer obj "")))
    (assert (equalp (get-by-json-pointer obj "/foo") '("bar" "baz")))
    (assert (equal (get-by-json-pointer obj "/foo/0") "bar"))
    (assert (equal (get-by-json-pointer obj "/") 0))
    (assert (equal (get-by-json-pointer obj "/a~1b") 1))
    (assert (equal (get-by-json-pointer obj "/c%d") 2))
    (assert (equal (get-by-json-pointer obj "/e^f") 3))
    (assert (equal (get-by-json-pointer obj "/g|h") 4))
    (assert (equal (get-by-json-pointer obj "/i\\j") 5))
    (assert (equal (get-by-json-pointer obj "/k\"l") 6))
    (assert (equal (get-by-json-pointer obj "/ ") 7))
    (assert (equal(get-by-json-pointer obj "/m~0n") 8)))
  t)

(1am:test test-top-page-runner
  (with-current-json-reader ('cl-json:decode-json-from-string)
    (1am:is (test-top-page))))
