# Abstract

A JSON Pointer ( [RFC6901](https://tools.ietf.org/html/rfc6901) ) implementation for Common Lisp.

This libary aims to be independent from any JSON libraries (as much as possible).

# News

- (2022-07-31) Added supports for com.inuoe.jzon, shasht, trivial-json-codec, boost-json, and json-lib.

# License

The MIT License. See LICENSE file.

# Loading

## Load by quicklisp

[![Quicklisp](http://quickdocs.org/badge/cl-json-pointer.svg)](http://quickdocs.org/cl-json-pointer/)

```lisp
(ql:quickload "cl-json-pointer")
```

## or, Load manually

### Libraries depending on

* alexandria
* closer-mop

This library itself does not depend on any JSON libraries.
You can work with your favorite one.

Current supported JSON libs are (alphabetical order):

- boost-json
- cl-json
- com.gigamonkeys.json
- com.inuoe.jzon
- jonathan (as `:plist` only)
- json-lib
- json-streams
- jsown
- shasht
- st-json
- trivial-json-codec (`deserialize-json` and `deserialize-json-raw`)
- yason

### Loading

```lisp
(asdf:load-asd "cl-json-pointer.asd")
(asdf:load-system :cl-json-pointer)
```

### Running tests.

The test code depends additinal libraries:

- All JSON Libraries enumerated above.
- 1am
- named-readtable

For running tests, do below additionally.

```lisp
(asdf:load-asd "cl-json-pointer-test.asd")
(asdf:test-system :cl-json-pointer)
```

# Examples

## get opetations (with st-json)

```lisp
(in-package :cl-user)
(use-package :cl-json-pointer)

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

(let ((obj (st-json:read-json-from-string *rfc6901-example*))
	  (cl-json-pointer:*json-object-flavor* :st-json))
  (eql obj (get-by-json-pointer obj "")) ; => T
  (get-by-json-pointer obj "/foo")	 ; => ("bar" "baz")
  (get-by-json-pointer obj "/foo/0")	 ; => "bar"
  (get-by-json-pointer obj "/")		 ; => 0
  (get-by-json-pointer obj "/a~1b")	 ; => 1
  (get-by-json-pointer obj "/c%d")	 ; => 2
  (get-by-json-pointer obj "/e^f")	 ; => 3
  (get-by-json-pointer obj "/g|h")	 ; => 4
  (get-by-json-pointer obj "/i\\j")	 ; => 5
  (get-by-json-pointer obj "/k\"l")	 ; => 6
  (get-by-json-pointer obj "/ ")	 ; => 7
  (get-by-json-pointer obj "/m~0n")	 ; => 8
  )
```

## set operations (with cl-json)

### setting to an object

```lisp

;;; Uses *rfc6901-example* above.

(defparameter *obj*
  (cl-json:decode-json-from-string *rfc6901-example*))
  
(get-by-json-pointer *obj* "/hoge" :flavor :cl-json) ; => nil
(exists-p-by-json-pointer *obj* "/hoge" :flavor :cl-json) ; => nil


;; Sets into "hoge" field.

(setf *obj*
	(set-by-json-pointer *obj* "/hoge" "something" :flavor :cl-json))

(get-by-json-pointer *obj* "/hoge" :flavor :cl-json) ; => "something"
(exists-p-by-json-pointer *obj* "/hoge" :flavor :cl-json) ; => T


;; `update-by-json-pointer' is a modify macro of `set-by-json-pointer`.

(update-by-json-pointer *obj* "/hoge" "something-2" :flavor :cl-json)

(get-by-json-pointer *obj* "/hoge" :flavor :cl-json) ; => "something-2"
(exists-p-by-json-pointer *obj* "/hoge" :flavor :cl-json) ; => T


;; setf to `(get-by-json-pointer ...)' can also be used.

(setf (get-by-json-pointer *obj* "/hoge" :flavor :cl-json) "fuga")

(get-by-json-pointer *obj* "/hoge" :flavor :cl-json) ; => "fuga"

```

### setting to an array


``` lisp

;; setting to array with index

(defparameter *obj*
  (cl-json:decode-json-from-string *rfc6901-example*))
  
(setf *json-object-flavor* :cl-json)  ; defaults :flavor to :cl-json

(get-by-json-pointer *obj* "/foo")	 ; => ("bar" "baz")

(update-by-json-pointer *obj* "/foo/0" "zero")
(update-by-json-pointer *obj* "/foo/1" "one")

(get-by-json-pointer *obj* "/foo")	 ; => ("zero" "one")


;; adding to an array tail with index

(exists-p-by-json-pointer *obj* "/foo/2") ; => NIL

(update-by-json-pointer *obj* "/foo/3" "three")

(get-by-json-pointer *obj* "/foo/3") ; => "three"
(exists-p-by-json-pointer *obj* "/foo/3") ; => T

(get-by-json-pointer *obj* "/foo/2") ; => NIL
(exists-p-by-json-pointer *obj* "/foo/2") ; => T


;; pushing to array tail with '-'

(exists-p-by-json-pointer *obj* "/foo/4") ; => NIL

(update-by-json-pointer *obj* "/foo/-" "four")

(get-by-json-pointer *obj* "/foo")	 ; => ("zero" "one" NIL "three" "four")
(exists-p-by-json-pointer *obj* "/foo/4") ; => T
```

## delete operations

### deleting from an object (with jsown)

```lisp

;;; Uses *rfc6901-example* above.

(defparameter *obj*
  (jsown:parse *rfc6901-example*))

(setf cl-json-pointer:*json-object-flavor* :jsown)


(get-by-json-pointer *obj* "/m~0n") ; => 8

(setf *obj*
	(delete-by-json-pointer *obj* "/m~0n"))

(get-by-json-pointer *obj* "/m~0n") ; => NIL
(exists-p-by-json-pointer *obj* "/m~0n") ; => NIL


;; `deletef-by-json-pointer' is a modify macro of `delete-by-json-pointer`.

(get-by-json-pointer *obj* "/ ") ; => 7

(deletef-by-json-pointer *obj* "/ ")

(get-by-json-pointer *obj* "/ ") ; => NIL
(exists-p-by-json-pointer *obj* "/ ") ; => NIL

```

### Deleting from an array (with yason)

```lisp

;;; Uses "cl-json-pointer/synonyms" system.
;;; This provides 'cljsp' package contains shorter symbols.

(asdf:load-system :cl-json-pointer/synonyms)


(defparameter *obj*
  (yason:parse *rfc6901-example*))

(setf cl-json-pointer:*json-object-flavor* :yason)


(cljsp:get *obj* "/foo")	 ; => ("bar" "baz")

(cljsp:deletef *obj* "/foo/0")
(cljsp:get *obj* "/foo")	 ; => ("baz")
```

# API

These symbols are exported from the `cl-json-pointer` package.
Please see their docstring.

- `parse-json-pointer`
- `*json-object-flavor*`
- `get-by-json-pointer`
- `exists-p-by-json-pointer`
- `set-by-json-pointer`
- `update-by-json-pointer`
- `delete-by-json-pointer`
- `deletef-by-json-pointer`

## symbols of `cl-json-pointer/synonyms`

Another "cl-json-pointer/synonyms" system provides "cljsp" package.
This package contains some shorter symbols.

For using this, please evaluate:
```lisp
(asdf:load-system :cl-json-pointer/synonyms)
```

After that, 'cljsp' package will be defined. It exports these symbols:

- `parse`
- `get`
- `exists-p`
- `set`
- `update`
- `delete`
- `deletef`

# See Also

## Related libraries

- [`jsown:val`](https://github.com/madnificent/jsown) functionalities.
  (I think, I cound simply depend only `jsown` for implementing RFC6901.)
  
- [`access`](https://github.com/AccelerationNet/access/) library
  traverses many kind of Lisp objects like cl-json-pointer do.

- [`jsown-utils`](https://github.com/muyinliu/jsown-utils/) has some
  accessors works like JSON pointers.

## Reviews

- [cl-json-pointer review in "Review of CL Json Libraries"](https://sabracrolleton.github.io/json-review#cl-json-pointer) by [@sabracrolleton](https://github.com/sabracrolleton).
- [cl-json-pointer review in "Lisp Project of the Day"](https://40ants.com/lisp-project-of-the-day/2020/08/0158-cl-json-pointer.html) by [@40ants](https://github.com/40ants).
