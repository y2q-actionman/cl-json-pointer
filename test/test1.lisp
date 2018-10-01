;;; Test codes by:
;;; https://github.com/janl/node-jsonpointer

(in-package :cl-json-pointer/test)
(in-readtable cjp-test-syntax)

;;; Test cases in top page.

(define-constant +test1-top-page+
    #{ "foo": 1, "bar": { "baz": 2}, "qux": [3, 4, 5]}
    :test 'equal)

(1am:test test1-top-page
  (let ((obj (read-json-string +test1-top-page+)))
    (1am:is (equal (get-by-json-pointer obj "/foo") 1))
    (1am:is (equal (get-by-json-pointer obj "/bar/baz") 2))
    (1am:is (equal (get-by-json-pointer obj "/qux/0") 3))
    (1am:is (equal (get-by-json-pointer obj "/qux/1") 4))
    (1am:is (equal (get-by-json-pointer obj "/qux/2") 5))
    (1am:is (not (get-by-json-pointer obj "/quo")))

    (update-by-json-pointer obj "/foo" 6)
    (1am:is (equal (get-by-json-pointer obj "/foo") 6))
    (update-by-json-pointer  obj "/foo" 7)
    (1am:is (equal (get-by-json-pointer obj "/foo") 7))

    (update-by-json-pointer obj "/qux/-" 6)
    (1am:is (equalp (get-by-json-pointer obj "/qux")
		    (current-json-reader-array-etypecase
		      (list '(3 4 5 6))
		      (array #(3 4 5 6)))))
    (update-by-json-pointer obj "/qux/-" 99)
    (1am:is (equalp (get-by-json-pointer obj "/qux")
		    (current-json-reader-array-etypecase
		      (list '(3 4 5 6 99))
		      (array #(3 4 5 6 99)))))

    (let ((pointer (parse-json-pointer "/foo")))
      (1am:is (equal (get-by-json-pointer obj pointer) 7))
      (update-by-json-pointer obj pointer 999)
      (1am:is (equal (get-by-json-pointer obj pointer) 999)))))

;;; Test cases in test.js

(define-constant +test1-obj+
  #{
    "a": 1,
    "b": {
      "c": 2
    },
    "d": {
      "e": [{ "a": 3 }, { "b": 4 }, { "c": 5 }]
    }
  }
  :test 'equal)

(1am:test test1-obj
  (let ((obj (read-json-string +test1-obj+)))
    (1am:is (equal (cljsp:get obj "/a") 1))
    (1am:is (equal (cljsp:get obj "/b/c") 2))
    (1am:is (equal (cljsp:get obj "/d/e/0/a") 3))
    (1am:is (equal (cljsp:get obj "/d/e/1/b") 4))
    (1am:is (equal (cljsp:get obj "/d/e/2/c") 5))

    (cljsp:update obj "/a" 2)
    (1am:is (equal (cljsp:get obj "/a") 2))
    (cljsp:update obj "/b/c" 3)
    (1am:is (equal (cljsp:get obj "/b/c") 3))
    (cljsp:update obj "/d/e/0/a" 4)
    (1am:is (equal (cljsp:get obj "/d/e/0/a") 4))
    (cljsp:update obj "/d/e/1/b" 5)
    (1am:is (equal (cljsp:get obj "/d/e/1/b") 5))
    (cljsp:update obj "/d/e/2/c" 6)
    (1am:is (equal (cljsp:get obj "/d/e/2/c") 6))

    ;; set nested properties
    (cljsp:update obj "/f/g/h/i" 6)
    (1am:is (equal (cljsp:get obj "/f/g/h/i")  6))

    ;; set an array
    (cljsp:update obj "/f/g/h/foo/-" "test")
    (let ((arr (cljsp:get obj  "/f/g/h/foo")))
      ;; TODO: add a way to specify type.
      ;; (1am:is (typep arr 'array))
      (1am:is (equal (elt arr 0) "test")))

    ;; can set `null` as a value
    (cljsp:update obj "/f/g/h/foo/0" nil)
    (1am:is (null (cljsp:get obj "/f/g/h/foo/0")))
    (cljsp:update obj "/b/c" nil)
    (1am:is (null (cljsp:get obj "/b/c")))

    (1am:is (equalp (cljsp:get obj "") obj))
    (1am:signals cl-json-pointer:json-pointer-error
      (cljsp:get obj "a"))
    (1am:signals cl-json-pointer:json-pointer-error
      (cljsp:get obj "a/"))

    ;; delete operations.
    ;; (In JS: can unset values with `undefined`)
    (cljsp:delete obj "/a")
    (1am:is (not (cljsp:exists-p obj  "/a")))
    (cljsp:delete obj "/d/e/1")
    (current-json-reader-array-etypecase
      (list
       (1am:is (cljsp:exists-p obj "/d/e/0"))
       (1am:is (cljsp:exists-p obj "/d/e/0/a"))
       (1am:is (cljsp:exists-p obj "/d/e/1"))
       (1am:is (not (cljsp:exists-p obj "/d/e/1/b")))
       (1am:is (cljsp:exists-p obj "/d/e/1/c"))
       (1am:is (not (cljsp:exists-p obj "/d/e/2"))))
      (array
       ;; In my implementation, deleting from array only sets `nil', so the element still exists.
       (1am:is (null (cljsp:get obj "/d/e/1")))
       (1am:is (cljsp:exists-p obj "/d/e/1"))))

    ;; returns `undefined` when path extends beyond any existing objects
    (1am:is (not (cljsp:exists-p obj "/x/y/z")))))


(define-constant +test1-complex-keys+
  #{
    "a/b": {
      "c": 1
    },
    "d": {
      "e/f": 2
    },
    "~1": 3,
    "01": 4
  }
  :test 'equal)

(1am:test test1-complex-keys
  (let ((obj (read-json-string +test1-complex-keys+)))
    (1am:is (equal (cljsp:get obj "/a~1b/c") 1))
    (1am:is (equal (cljsp:get obj "/d/e~1f") 2))
    (1am:is (equal (cljsp:get obj "/~01") 3))
    (1am:is (equal (cljsp:get obj "/01") 4))
    (1am:is (equal (cljsp:get obj "/a/b/c") nil))
    (1am:is (equal (cljsp:exists-p obj "/a/b/c") nil))
    (1am:is (equal (cljsp:get obj "/~1") nil))
    (1am:is (equal (cljsp:exists-p obj "/~1") nil))))


(define-constant +test1-ary+
    #[ "zero", "one", "two" ]
  :test 'equal)

(1am:test test1-ary
  (let ((ary (read-json-string +test1-ary+)))
    ;; draft-ietf-appsawg-json-pointer-08 has special array rules
    (1am:signals cl-json-pointer:json-pointer-error
      (cljsp:get ary "/01"))
    (cljsp:update ary "/-" "three")
    (1am:is (equal (elt ary 3) "three"))))


(define-constant +test1-example+
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

(1am:test test1-example
  (let ((example (read-json-string +test1-example+)))
    (1am:is (equal (cljsp:get example "") example))

    (let ((ans (cljsp:get example "/foo")))
      (1am:is (equal (length ans) 2))
      (1am:is (equal (elt ans 0) "bar"))
      (1am:is (equal (elt ans 1) "baz")))

    (1am:is (equal (cljsp:get example "/foo/0") "bar"))
    (1am:is (equal (cljsp:get example "/") 0))
    (1am:is (equal (cljsp:get example "/a~1b") 1))
    (1am:is (equal (cljsp:get example "/c%d") 2))
    (1am:is (equal (cljsp:get example "/e^f") 3))
    (1am:is (equal (cljsp:get example "/g|h") 4))
    (1am:is (equal (cljsp:get example "/i\\j") 5))
    (1am:is (equal (cljsp:get example "/k\"l") 6))
    (1am:is (equal (cljsp:get example "/ ") 7))
    (1am:is (equal (cljsp:get example "/m~0n") 8))))


(define-constant +test1-a+
    #{"foo": "bar"}
    :test 'equal)

(1am:test test1-a
  (let ((a (read-json-string +test1-a+))
	(pointer (cljsp:parse "/foo")))
    (1am:is (equal (cljsp:get a pointer) "bar"))
    (cljsp:update a pointer "test")
    (1am:is (equal (cljsp:get a pointer) "test"))

    (let ((result-obj (read-json-string #{"foo": "test"})))
      (1am:is (equalp a result-obj)))))
