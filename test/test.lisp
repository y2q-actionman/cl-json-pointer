(in-package :cl-json-pointer/test)

(alexandria:define-constant +rfc6901-example+
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
}"
  ;; TODO: use reader macro.
  :test 'equal)

(alexandria:define-constant +rfc6901-example-keys+
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
  (loop for cas in +rfc6901-example-keys+
     do (format t "~&case ~S, parsed ~S~%"
		cas (cl-json-pointer::parse-json-pointer cas))))

(defun test-traverse-json ()
  (let ((json (read-json-string +rfc6901-example+)))
    (loop for cas in +rfc6901-example-keys+
       do (format t "~&pointer ~S, result ~S~%"
		    cas (get-by-json-pointer json cas)))))


(defclass test-class ()
  ((hoge :initform 'hoge-value)))

(defun test-traverse-json-2 ()
  (let ((obj (make-instance 'test-class))
	(ptr "/hoge"))
    (get-by-json-pointer obj ptr)))


(defun run ()
  (test-parse-json-pointer)
  (test-traverse-json)
  (test-traverse-json-2))
