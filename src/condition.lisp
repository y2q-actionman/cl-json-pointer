(in-package :cl-json-pointer)

(define-condition json-pointer-error (simple-error)
  ()
  (:documentation "The root object of all errors related to cl-json-pointer"))

(define-condition json-pointer-parse-error (json-pointer-error)
  ()
  (:documentation "Thrown by `parse-json-pointer'"))

(define-condition json-pointer-bad-reference-token-error (json-pointer-error)
  ((rtoken :initarg :reference-token :initform nil))
  (:report (lambda (c stream)
	     (format stream (simple-condition-format-control c)
		     (slot-value c 'rtoken))))
  (:documentation "Errors related to the json-pointer syntax")
  (:default-initargs
      :format-control "Bad reference token (~A)"))

(define-condition json-pointer-bad-reference-token-0-used-error (json-pointer-bad-reference-token-error)
  ()
  (:default-initargs
      :format-control "reference token (~A) must not start with '0' when used as an index"))

(define-condition json-pointer-bad-reference-token-not-numeric-error (json-pointer-bad-reference-token-error)
  ()
  (:default-initargs
      :format-control "reference token (~A) cannot be read as index"))

(define-condition json-pointer-access-error (json-pointer-error)
  ()
  (:documentation "Errors at traversing an object"))
