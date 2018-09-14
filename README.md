# Abstract

JSON Pointer, defined in [RFC6901](https://tools.ietf.org/html/rfc6901), processor for Common Lisp.

Aiming to:
* Independent from any JSON libraries (as much as possible).
* Usable for built-in Lisp objects -- lists, arrays, CLOS objects, hash-tables, etc.

# License

The MIT License. See LICENSE file.

# Loading

## Libraries depending on

* asdf
* alexandria
* closer-mop

This library itself does not depend on any JSON libraries.
You can work with your favorite one.

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

(let ((obj (cl-json:decode-json-from-string *rfc6901-example*)))
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

(TODO: add 'set' examples)

(TODO: add Lisp object examples)

# API

(stub)
