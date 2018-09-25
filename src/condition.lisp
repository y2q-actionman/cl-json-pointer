(in-package :cl-json-pointer)

(define-condition json-pointer-error (simple-error)
  ())

(define-condition json-pointer-parse-error (json-pointer-error)
  ())

(define-condition json-pointer-not-found-error (json-pointer-error)
  ())

(define-condition json-pointer-bad-reference-token-error (json-pointer-error)
  ())

(define-condition json-pointer-access-error (json-pointer-error)
  ())
