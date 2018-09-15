;;; Test codes by:
;;; https://github.com/janl/node-jsonpointer

(in-package :cl-json-pointer/test)
(in-readtable cjp-test-syntax)

;;; Test cases in top page.

(define-constant +test1-top-page+
    #{ "foo": 1, "bar": { "baz": 2}, "qux": [3, 4, 5]}
    :test 'equal)

(defun test1-top-page (&aux (obj (read-json-string +test1-top-page+)))
  (assert (equal (get-by-json-pointer obj "/foo") 1))
  (assert (equal (get-by-json-pointer obj "/bar/baz") 2))
  (assert (equal (get-by-json-pointer obj "/qux/0") 3))
  (assert (equal (get-by-json-pointer obj "/qux/1") 4))
  (assert (equal (get-by-json-pointer obj "/qux/2") 5))
  (assert (not (get-by-json-pointer obj "/quo")))

  (set-by-json-pointer obj "/foo" 6)
  (assert (equal (get-by-json-pointer obj "/foo") 6))
  (setf (get-by-json-pointer obj "/foo") 7)
  (assert (equal (get-by-json-pointer obj "/foo") 7))

  (set-by-json-pointer obj "/qux/-" 6)
  (assert (equalp (get-by-json-pointer obj "/qux")
		  #(3 4 5 6)))
  (setf (get-by-json-pointer obj "/qux/-") 99)
  (assert (equalp (get-by-json-pointer obj "/qux")
		  #(3 4 5 6 99)))

  (let ((pointer (parse-json-pointer "/foo")))
    (assert (equal (get-by-json-pointer obj pointer) 7))
    (setf (get-by-json-pointer obj pointer) 999)
    (assert (equal (get-by-json-pointer obj pointer) 999)))

  t)

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

(defun test1-obj (&aux (obj (read-json-string +test1-obj+)))
  (assert (equal (cljsp:get obj "/a") 1))
  (assert (equal (cljsp:get obj "/b/c") 2))
  (assert (equal (cljsp:get obj "/d/e/0/a") 3))
  (assert (equal (cljsp:get obj "/d/e/1/b") 4))
  (assert (equal (cljsp:get obj "/d/e/2/c") 5))

  (cljsp:set obj "/a" 2)
  (assert (equal (cljsp:get obj "/a") 2))
  (cljsp:set obj "/b/c" 3)
  (assert (equal (cljsp:get obj "/b/c") 3))
  (cljsp:set obj "/d/e/0/a" 4)
  (assert (equal (cljsp:get obj "/d/e/0/a") 4))
  (cljsp:set obj "/d/e/1/b" 5)
  (assert (equal (cljsp:get obj "/d/e/1/b") 5))
  (cljsp:set obj "/d/e/2/c" 6)
  (assert (equal (cljsp:get obj "/d/e/2/c") 6))

  ;; ;; set nested properties
  ;; (cljsp:set obj "/f/g/h/i" 6)
  ;; (assert (equal (cljsp:get obj "/f/g/h/i")  6))

  ;; ;; set an array
  ;; (cljsp:set obj "/f/g/h/foo/-" "test")
  ;; (let ((arr (cljsp:get obj  "/f/g/h/foo")))
  ;;   (assert (typep arr 'array))
  ;;   (assert (equal (aref arr 0) "test")))

  ;; can set `null` as a value
  ;; (cljsp:set obj "/f/g/h/foo/0" nil)
  ;; (assert (null (cljsp:get obj "/f/g/h/foo/0")))
  ;; (cljsp:set obj "/b/c/" nil)
  ;; (assert (null (cljsp:get obj "/b/c")))

  (assert (equalp (cljsp:get obj "") obj))
  (assert-condition (cljsp:get obj "a"))
  (assert-condition (cljsp:get obj "a/"))

  ;; can unset values with `undefined`
  ;; (cljsp:set obj "/a" undefined)
  ;; (assert (not (cljsp:exists-p obj  "/a")))
  ;; (cljsp:set obj "/d/e/1" undefined)
  ;; (assert (not (cljsp:exists-p obj "/d/e/1")))

  ;; returns `undefined` when path extends beyond any existing objects
  (assert (not (cljsp:exists-p obj "/x/y/z")))

  t)


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

(defun test1-complex-keys (&aux (obj (read-json-string +test1-complex-keys+)))
  (assert (equal (cljsp:get obj "/a~1b/c") 1))
  (assert (equal (cljsp:get obj "/d/e~1f") 2))
  (assert (equal (cljsp:get obj "/~01") 3))
  (assert (equal (cljsp:get obj "/01") 4))
  (assert (equal (cljsp:get obj "/a/b/c") nil))
  (assert (equal (cljsp:exists-p obj "/a/b/c") nil))
  (assert (equal (cljsp:get obj "/~1") nil))
  (assert (equal (cljsp:exists-p obj "/~1") nil))

  t)


(define-constant +test1-ary+
    #[ "zero", "one", "two" ]
  :test 'equal)

(defun test1-ary (&aux (ary (read-json-string +test1-ary+)))
  ;; draft-ietf-appsawg-json-pointer-08 has special array rules
  (assert-condition (cljsp:get ary "/01"))

  ;; (cljsp:set ary "/-" "three")
  ;; (assert (equal (aref ary 3) "three"))

  t)


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

(defun test1-example (&aux (example (read-json-string +test1-example+)))
  (assert (equal (cljsp:get example "") example))

  (let ((ans (cljsp:get example "/foo")))
    (assert (equal (length ans) 2))
    (assert (equal (aref ans 0) "bar"))
    (assert (equal (aref ans 1) "baz")))

  (assert (equal (cljsp:get example "/foo/0") "bar"))
  (assert (equal (cljsp:get example "/") 0))
  (assert (equal (cljsp:get example "/a~1b") 1))
  (assert (equal (cljsp:get example "/c%d") 2))
  (assert (equal (cljsp:get example "/e^f") 3))
  (assert (equal (cljsp:get example "/g|h") 4))
  (assert (equal (cljsp:get example "/i\\j") 5))
  (assert (equal (cljsp:get example "/k\"l") 6))
  (assert (equal (cljsp:get example "/ ") 7))
  (assert (equal (cljsp:get example "/m~0n") 8))

  t)


(define-constant +test1-a+
    #{"foo": "bar"}
    :test 'equal)

(defun test1-a (&aux (a (read-json-string +test1-a+)))
  (let ((pointer (cljsp:parse "/foo")))
    (assert (equal (cljsp:get a pointer) "bar"))
    (cljsp:set a pointer "test")
    (assert (equal (cljsp:get a pointer) "test"))

    (let ((result-obj (read-json-string #{"foo": "test"})))
      (assert (equalp a result-obj)))

    t))


(defun test1-run ()
  (test1-top-page)
  (test1-obj)
  (test1-complex-keys)
  (test1-ary)
  (test1-example)
  (test1-a)
  t)
