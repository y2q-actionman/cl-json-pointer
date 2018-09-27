(in-package :cl-json-pointer/test)
(in-readtable cjp-test-syntax)

(define-constant +rfc6901-example+
  #{
   "foo": ["bar", "baz"],
   "": 0,
   "a/b": 1,
   "c%d": 2,
   "e^f": 3,
   "g|h": 4,
   "i\\j": 5,
   "k\"l": 6,
   " ": 7,
   "m~n": 8
}
  :test 'equal)

(define-constant +rfc6901-example-keys+
  '(""
    "/foo"
    "/foo/0"
    "/"
    "/a~1b"
    "/c%d"
    "/e^f"
    "/g|h"
    "/i\\j"
    "/k\"l"
    "/ "
    "/m~0n")
  :test 'equal)


(1am:test test0-parse-json-pointer
  (1am:is (equal (mapcar #'parse-json-pointer +rfc6901-example-keys+)
		 '(()
		   ("foo")
		   ("foo" "0")
		   ("")
		   ("a/b")
		   ("c%d")
		   ("e^f")
		   ("g|h")
		   ("i\\j")
		   ("k\"l")
		   (" ")
		   ("m~n")))))

(1am:test test0-traverse-json
  (let ((json (read-json-string +rfc6901-example+)))
    (loop for cas in +rfc6901-example-keys+
       as obj = (get-by-json-pointer json cas)
       for expected in `(,json
			 #("bar" "baz") "bar"
			 0 1 2 3 4 5 6 7 8)
       always (1am:is (equalp obj expected)))))

(defclass test-class ()
  ((hoge :initform 'hoge-value)))

(1am:test test-traverse-json-2
  (let ((obj (make-instance 'test-class))
	(ptr "/hoge"))
    (1am:is (equal (get-by-json-pointer obj ptr)
		   'hoge-value))))

(1am:test test-traverse-json-3 ()
  (let ((obj '((:a . 1) (:b . 2))))
    (1am:is (equal (get-by-json-pointer obj "/a") 1))
    (1am:is (equal (get-by-json-pointer obj "/b") 2))
    (let ((setf-ed-obj obj))
      (setf (get-by-json-pointer setf-ed-obj "/c") 3) ; This `setf' updates `setf-ed-obj' itself.
      (1am:is (not (equal obj setf-ed-obj)))
      (1am:is (equal (get-by-json-pointer setf-ed-obj "/c") 3)))))
