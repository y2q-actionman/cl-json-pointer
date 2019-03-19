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

;; This test is intended only for `cl-json'.
(defun test-top-page ()			
  (let ((obj (cl-json:decode-json-from-string *rfc6901-example*)))
    (1am:is (eql obj (get-by-json-pointer obj "")))
    (1am:is (equalp (get-by-json-pointer obj "/foo") '("bar" "baz")))
    (1am:is (equal (get-by-json-pointer obj "/foo/0") "bar"))
    (1am:is (equal (get-by-json-pointer obj "/") 0))
    (1am:is (equal (get-by-json-pointer obj "/a~1b") 1))
    (1am:is (equal (get-by-json-pointer obj "/c%d") 2))
    (1am:is (equal (get-by-json-pointer obj "/e^f") 3))
    (1am:is (equal (get-by-json-pointer obj "/g|h") 4))
    (1am:is (equal (get-by-json-pointer obj "/i\\j") 5))
    (1am:is (equal (get-by-json-pointer obj "/k\"l") 6))
    (1am:is (equal (get-by-json-pointer obj "/ ") 7))
    (1am:is (equal (get-by-json-pointer obj "/m~0n") 8))))

(1am:test test-top-page-runner
  (when (eq *current-json-reader* 'cl-json:decode-json-from-string)
    (test-top-page)))
