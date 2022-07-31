(defpackage :cl-json-pointer/test-trivial-json-codec
  (:use :cl)
  (:export #:Payload
           #:Message))

(in-package :cl-json-pointer/test-trivial-json-codec)

;;; Copied from https://gitlab.com/ediethelm/trivial-json-codec/-/blob/master/README.md

(defclass Payload ()
  ())

(defclass SimplePayload (Payload)
  ((value :type integer
          :initarg :value)))

(defclass ComplicatedPayload (Payload)
  ((value :type string
          :initarg :value)
   (additional-info :type string
		    :initarg :additional-info)
   (message-id :type trivial-utilities:positive-fixnum
	       :initarg :message-id)))

(defclass DifferentPayload (Payload)
  ((cargo :type fixnum
          :initarg :cargo)))

(defclass Message ()
  ((uid :initarg :uid
	:initform nil
	:accessor uid)
   (payload :type (or null Payload)
	    :initarg :payload
	    :accessor payload)))

(c2mop:ensure-finalized (find-class 'Payload))
(c2mop:ensure-finalized (find-class 'SimplePayload))
(c2mop:ensure-finalized (find-class 'ComplicatedPayload))
(c2mop:ensure-finalized (find-class 'DifferentPayload))
(c2mop:ensure-finalized (find-class 'Message))


(in-package :cl-json-pointer/test)

(defun test-trivial-json-codec ()
  ;; Copied from https://gitlab.com/ediethelm/trivial-json-codec/-/blob/master/README.md
  (let ((obj (trivial-json-codec:deserialize-json
              "{ \"UID\" : 1,  \"PAYLOAD\" : { \"VALUE\" : 12345}}"
              :class (find-class 'cl-json-pointer/test-trivial-json-codec:Message))))
    (1am:is (typep obj 'cl-json-pointer/test-trivial-json-codec:Message))
    (1am:is (equal (get-by-json-pointer obj "/UID") 1))
    (1am:is (equal (get-by-json-pointer obj "/PAYLOAD/VALUE") 12345))
    (1am:is (typep (get-by-json-pointer obj "/PAYLOAD") 'cl-json-pointer/test-trivial-json-codec:Payload)))
  (let ((obj (trivial-json-codec:deserialize-json
              "{ \"UID\" : 2,  \"PAYLOAD\" : { \"VALUE\" : \"abc\",  \"ADDITIONAL-INFO\" : \"1234\",  \"MESSAGE-ID\" : 17}}"
              :class (find-class 'cl-json-pointer/test-trivial-json-codec:Message))))
    (1am:is (typep obj 'cl-json-pointer/test-trivial-json-codec:Message))
    (1am:is (equal (get-by-json-pointer obj "/UID") 2))
    (1am:is (equal (get-by-json-pointer obj "/PAYLOAD/VALUE") "abc"))
    (1am:is (equal (get-by-json-pointer obj "/PAYLOAD/ADDITIONAL-INFO") "1234"))
    (1am:is (equal (get-by-json-pointer obj "/PAYLOAD/MESSAGE-ID") 17))
    (let ((payload (get-by-json-pointer obj "/PAYLOAD")))
      (1am:is (typep payload 'cl-json-pointer/test-trivial-json-codec:Payload))
      (1am:is (eq (get-by-json-pointer obj "/PAYLOAD/VALUE") (get-by-json-pointer payload "/VALUE")))
      (1am:is (eq (get-by-json-pointer obj "/PAYLOAD/ADDITIONAL-INFO") (get-by-json-pointer payload "/ADDITIONAL-INFO")))
      (1am:is (eq (get-by-json-pointer obj "/PAYLOAD/MESSAGE-ID") (get-by-json-pointer payload "/MESSAGE-ID")))))
  (let ((obj (trivial-json-codec:deserialize-json
              "{ \"UID\" : 2,  \"PAYLOAD\" : { \"CARGO\" : -147}}"
              :class (find-class 'cl-json-pointer/test-trivial-json-codec:Message))))
    (1am:is (typep obj 'cl-json-pointer/test-trivial-json-codec:Message))
    (1am:is (equal (get-by-json-pointer obj "/UID") 2))
    (1am:is (equal (get-by-json-pointer obj "/PAYLOAD/CARGO") -147)))
  ;;
  t)

(1am:test test-trivial-json-codec-1am
  (unless (eq *json-object-flavor* :trivial-json-codec)
    (return-from test-trivial-json-codec-1am t))
  (format t "~&trivial-json-codec test runs only on :trivial-json-codec flavor.")
  (test-trivial-json-codec))
