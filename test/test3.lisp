;;; Test codes by:
;;; https://github.com/manuelstofer/json-pointer/blob/master/test/test.js

(in-package :cl-json-pointer/test)
(in-readtable cjp-test-syntax)

(1am:test test3-get
  (let ((obj (make-instance 'standard-object)))
    (1am:is (eq obj (cljsp:get obj "")))))

(1am:test test3-set
  ;; If set to root, my impl simply sets into the root!
  (let ((obj nil))
    (setf (cljsp:get obj "") 'foo) 
    (1am:is (eq obj 'foo)))
  (let ((obj (acons :existing "bla" nil)))
    (setf (cljsp:get obj "/new-value/bla") :expected)
    (1am:is (eql (cljsp:get obj "/new-value/bla") :expected))
    (setf (cljsp:get obj "/first-level") :expected)
    (1am:is (eql (cljsp:get obj "/first-level") :expected)))
  (let ((obj nil))
    (setf (cljsp:get obj "/0/test/0") :expected)
    ;; intermediate structures
    (1am:is (cljsp:exists-p obj ""))
    (1am:is (cljsp:exists-p obj "/0"))
    (1am:is (cljsp:exists-p obj "/0/test"))
    ;; TODO: Types depend on nil-handling method.
    ;; (1am:is (arrayp (cljsp:get obj "")))
    ;; (1am:is (not (arrayp (cljsp:get obj "/0"))))
    ;; (1am:is (arrayp (cljsp:get obj "/0/test")))
    (1am:is (equal (cljsp:get obj "/0/test/0") :expected)))
  (let ((obj (list "foo")))
    (setf (cljsp:get obj "/-/test/-") :expected)
    ;; intermediate structures
    (1am:is (cljsp:exists-p obj ""))
    (1am:is (cljsp:exists-p obj "/1"))
    (1am:is (cljsp:exists-p obj "/1/test"))
    ;; TODO: Types depend on nil-handling method.
    ;; (1am:is (arrayp (cljsp:get obj "/")))
    (1am:is (length= 2 (cljsp:get obj "")))
    ;; (1am:is (not (arrayp (cljsp:get obj "/1"))))
    ;; (1am:is (arrayp (cljsp:get obj "/1/test")))
    (1am:is (equal (cljsp:get obj "/1/test/0") :expected))))

(1am:test test3-delete
  (current-json-reader-array-etypecase
    (list
     ;; This is the JS's original test. In my impl, this requires 'list' semantics!
     (let ((obj (read-json-string +rfc6901-example+))) ; see test0.lisp
       (1am:is (cljsp:delete obj "/foo/0"))
       (1am:is (equal (cljsp:get obj "/foo/0") "baz")))
     (let ((obj (read-json-string +rfc6901-example+)))
       (1am:is (cljsp:delete obj "/foo/1"))
       (1am:is (not (cljsp:exists-p obj "/foo/1")))))
    (array
     ;; My impl does not shrink arrays.
     (let* ((obj (read-json-string +rfc6901-example+))
	    (len (length (cljsp:get obj "/foo")))
	    (old1 (cljsp:get obj "/foo/1")))
       (1am:is (cljsp:delete obj "/foo/0"))
       (1am:is (eq (cljsp:get obj "/foo/0") nil)) ; TODO: FIXME: This is depend on deleting op.
       (1am:is (eq (cljsp:get obj "/foo/1") old1))
       (1am:is (length= len (cljsp:get obj "/foo"))))
     (let* ((obj (read-json-string +rfc6901-example+))
	    (len (length (cljsp:get obj "/foo")))
	    (old0 (cljsp:get obj "/foo/0")))
       (1am:is (cljsp:delete obj "/foo/1"))
       (1am:is (eq (cljsp:get obj "/foo/0") old0))
       (1am:is (eq (cljsp:get obj "/foo/1") nil)) ; TODO: FIXME: This is depend on deleting op.
       (1am:is (length= len (cljsp:get obj "/foo"))))))) 

;;; nothing for 'dict'

(define-constant +test3-has-obj+
  #{
    "bla": {
        "test": "expected"
    },
    "foo": [["hello"]],
    "abc": "bla"
  }
  :test 'equal)

(1am:test test3-has
  (let ((obj (read-json-string +test3-has-obj+)))
    (1am:is (cljsp:exists-p obj "/bla"))
    (1am:is (cljsp:exists-p obj "/abc"))
    (1am:is (cljsp:exists-p obj "/foo/0/0"))
    (1am:is (cljsp:exists-p obj "/bla/test"))
    (1am:is (not (cljsp:exists-p obj "/not-existing")))
    (1am:is (not (cljsp:exists-p obj "/not-existing/bla")))
    (1am:is (not (cljsp:exists-p obj "/test/1/bla")))
    (1am:is (not (cljsp:exists-p obj "/bla/test1")))))

;;; nothing for 'walk'

(1am:test test3-parse
  ;; FIXME: too fragile
  (1am:is (equal (cljsp:parse "/bla")
		 '("bla")))
  (1am:is (equal (cljsp:parse "/hello~0bla/test~1bla")
		 '("hello~bla" "test/bla"))))

;;; nothing for 'compile'

;;; nothing for the 'convenience api'.
