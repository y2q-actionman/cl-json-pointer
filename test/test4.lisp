;;; Test codes by:
;;; https://github.com/WHenderson/json-pointer-rfc6901/tree/master/test

(in-package :cl-json-pointer/test)
(in-readtable cjp-test-syntax)

;;; nothing for 'escape', 'escapeFragment', 'unescape' 'unescapeFragment'

(1am:test test4-parse
  (1am:is (null (cljsp:parse ""))) ; my impl returns nil.
  (1am:signals cl-json-pointer:json-pointer-error
    (cljsp:parse "a"))
  (1am:is (equal (cljsp:parse "/a/b") '("a" "b")))
  (1am:is (equal (cljsp:parse "/~0/~1") '("~" "/"))))

(1am:test test4-parse-fragment
  (1am:is (null (cljsp:parse "#"))) ; my impl returns nil.
  (1am:signals cl-json-pointer:json-pointer-error
    (cljsp:parse "#a"))
  (1am:is (equal (cljsp:parse "#/a/b") '("a" "b")))
  (1am:is (equal (cljsp:parse "#/~0/~1") '("~" "/")))
  ;; TODO: required?
  #+ ()
  (1am:is (equal (cljsp:parse "#/~0%20/~1%20") '("~ " "/ "))))

(1am:test test4-parse-pointer
  (1am:signals cl-json-pointer:json-pointer-error
    (cljsp:parse "#" :accept-uri-fragment nil))
  (1am:signals cl-json-pointer:json-pointer-error
    (cljsp:parse "#a" :accept-uri-fragment nil))
  (1am:signals cl-json-pointer:json-pointer-error
    (cljsp:parse "#/a/b" :accept-uri-fragment nil))
  (1am:signals cl-json-pointer:json-pointer-error
    (cljsp:parse "#/~0/~1" :accept-uri-fragment nil)))

;;; nothing for 'isPointer', 'isFragment', 'compile', 'compilePointer',
;;; 'compileFragment', 'hasJsonProp', 'hasOwnProp', 'hasProp'.

;;; 'get' is same for RFC6901 example.

(define-constant +test4-has-object+
    #{
      "a": 1,
      "b": [2,3]
    }
    :test #'equal)

(1am:test test4-has
  (let ((obj (read-json-string +test4-has-object+)))
    (1am:is (cljsp:exists-p obj ""))
    (1am:is (cljsp:exists-p obj "/a"))
    (1am:is (cljsp:exists-p obj "/b/0"))
    (1am:is (not (cljsp:exists-p obj "/c")))
    (1am:is (not (cljsp:exists-p obj "/b/-")))))

(1am:test test4-set
  (let ((obj (read-json-string
	      #{})))
    (cljsp:update obj "" 1)
    (1am:is (eql obj 1)))
  (let ((obj (read-json-string
	      #{})))
    (cljsp:update obj "/a" 1)
    (1am:is (eql (cljsp:get obj "/a") 1)))
  (let ((obj (read-json-string
	      #{ "a": {} })))
    (cljsp:update obj "/a/b/c" 1)
    (1am:is (eql (cljsp:get obj "/a/b/c") 1)))
  (let ((obj (read-json-string
	      #[])))
    (cljsp:update obj "/-" 1)
    (1am:is (eql (cljsp:get obj "/0") 1)))
  (let ((obj (read-json-string
	      #[[]])))
    (cljsp:update obj "/0/-/-" 1)
    (1am:is (eql (cljsp:get obj "/0/0/0") 1))))

(1am:test test4-delete
  (let ((obj #{}))
    (setf obj (cljsp:delete obj ""))
    (1am:is (eql obj nil)))
  (let ((obj (read-json-string
	      #{ "a": 1 })))
    (setf obj (cljsp:delete obj "/a"))
    (1am:is (cljsp:exists-p obj ""))
    (1am:is (not (cljsp:exists-p obj "/a"))))
  (let ((obj (read-json-string
	      #{ "a": { "b": { "c": 1 } } })))
    (setf obj (cljsp:delete obj "/a/b/c"))
    (1am:is (cljsp:exists-p obj ""))
    (1am:is (cljsp:exists-p obj "/a"))
    (1am:is (cljsp:exists-p obj "/a/b"))
    (1am:is (not (cljsp:exists-p obj "/a/b/c"))))
  (let ((obj (read-json-string
	      #{ "a": 1 })))
    (1am:signals cl-json-pointer:json-pointer-error
      (cljsp:delete obj "/b")))
  (let ((obj (read-json-string
	      #[ 1 ])))
    (setf obj (cljsp:delete obj "/0"))
    (1am:is (cljsp:exists-p obj ""))	; the root always exists.
    (current-json-reader-array-etypecase
      (list (1am:is (not (cljsp:exists-p obj "/0"))))
      (array (1am:is (null (cljsp:get obj "/0"))))))
  (let ((obj (read-json-string
	      #[[[1]]])))
    (setf obj (cljsp:delete obj "/0/0/0"))
    (1am:is (cljsp:exists-p obj ""))
    (current-json-reader-array-etypecase
      (list (1am:is (not (cljsp:exists-p obj "/0")))
	    (1am:is (not (cljsp:exists-p obj "/0/0"))))
      (array (1am:is (cljsp:exists-p obj "/0"))
	     (1am:is (cljsp:exists-p obj "/0/0"))))
    (current-json-reader-array-etypecase
      (list (1am:is (not (cljsp:exists-p obj "/0/0/0"))))
      (array (1am:is (null (cljsp:get obj "/0/0/0"))))))
  (let ((obj (read-json-string
	      #[ 1 ])))
    (1am:signals cl-json-pointer:json-pointer-error
      (cljsp:delete obj "/-"))))

;;; nothing for 'simplified', 'bind', 'rebind', 'bound meta data'
