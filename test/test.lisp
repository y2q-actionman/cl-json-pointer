(in-package :cl-json-pointer)


(defvar *rfc6901-example-keys*
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
    "/m~0n"))

(defun test-parse-json-pointer ()
  (flet ((dt (cas)
	   (format t "~&case ~S, parsed ~S~%" cas (parse-json-pointer cas))))
    (loop for c in *rfc6901-example-keys*
       do (dt c))))

(defun test-traverse-json ()
  (let ((json (read-json-file "rfc6901-example.json")))
    (flet ((dt (cas)
	     (let* ((ptr (parse-json-pointer cas))
		    (result (traverse-json json ptr)))
	       (format t "~&pointer ~S, parsed-pointer ~S, result ~S~%"
		       cas ptr result))))
      (loop for c in *rfc6901-example-keys*
	 do (dt c)))))
