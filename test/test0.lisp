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


(defun test-parse-json-pointer ()
  (assert (equal (mapcar #'parse-json-pointer +rfc6901-example-keys+)
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
		   ("m~n"))))
  t)

(defun test-traverse-json (&aux (json (read-json-string +rfc6901-example+)))
  (loop for cas in +rfc6901-example-keys+
     as obj = (get-by-json-pointer json cas)
     for expected in `(,json
		       #("bar" "baz") "bar"
		       0 1 2 3 4 5 6 7 8)
     always (assert (equalp obj expected))))

(defclass test-class ()
  ((hoge :initform 'hoge-value)))

(defun test-traverse-json-2 ()
  (let ((obj (make-instance 'test-class))
	(ptr "/hoge"))
    (assert (equal (get-by-json-pointer obj ptr)
		   'hoge-value)))
  t)

(defun test0-run ()
  (and (test-parse-json-pointer)
       (test-traverse-json)
       (test-traverse-json-2))
  t)
