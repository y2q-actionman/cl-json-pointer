# Abstract

A JSON Pointer, defined in [RFC6901](https://tools.ietf.org/html/rfc6901), implementation for Common Lisp.

This libary aims to be independent from any JSON libraries (as much as possible).

# License

The MIT License. See LICENSE file.

# Loading

## Libraries depending on

* asdf
* alexandria
* closer-mop

This library itself does not depend on any JSON libraries.
You can work with your favorite one.

Current supported json libs are:

- cl-json
- st-json
- yason
- jsown
- jonathan (as `:plist` only)
- json-streams
- com.gigamonkeys.json


## Load

Currently, this is not quicklisp-ready. Please load manually.

```lisp
(load "cl-json-pointer.asd")
(asdf:load-system :cl-json-pointer)
```

## Running tests.

For running tests, do below additionally.
(CAUTION: Test codes depends on many JSON libraries!)

```lisp
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

# TODO

- Integration with [`jsown:val`](https://github.com/madnificent/jsown) functionalities.
  (I think, I cound simply depend only `jsown` for implementing RFC6901.)
  
- Integration with [`access`](https://github.com/AccelerationNet/access/) library.
