(in-package :cl-json-pointer/test)

(defmacro push-json-reader-alist (keyword function)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew (cons ,keyword ,function)
	      *json-reader-alist* :test #'equal)))

;;; cl-json
;; TODO: use fluid-object 

;; specially crafted cl-json's one. I make it default.
(defun read-json-string/cl-json-crafted (string)
  (cl-json:bind-custom-vars (:array-type 'vector)
    (let ((cl-json:*json-identifier-name-to-lisp* #'identity))
      (cl-json:decode-json-from-string string))))

(unless *current-json-reader*
  (setf *current-json-reader* 'read-json-string/cl-json-crafted))

(push-json-reader-alist t 'read-json-string/cl-json-crafted)
(push-json-reader-alist :cl-json 'cl-json:decode-json-from-string)

;;; st-json
(push-json-reader-alist :st-json 'st-json:read-json-from-string)

;;; yason
(push-json-reader-alist :yason 'yason:parse)

;; TODO: use variables
;; (*parse-json-arrays-as-vectors*)
;; (*parse-json-booleans-as-symbols*) 
;; (*parse-json-null-as-keyword*)
;; (*parse-object-key-fn*)

;;; jsown
(push-json-reader-alist :jsown 'jsown:parse)

;;; jonathan
(push-json-reader-alist :jonathan 'jonathan:parse)

;; TODO: `:as' variants: alist, array, hash-table

;;; json-streams
(push-json-reader-alist :json-streams 'json-streams:json-parse)

(defmacro json-streams-array-pop-prefix (js-array)
  `(when (eq *current-json-reader* 'json-streams:json-parse)
     (check-type ,js-array list)
     (assert (eq (pop ,js-array) :array))))

;;; com.gigamonkeys.json
(push-json-reader-alist :com.gigamonkeys.json 'com.gigamonkeys.json:parse-json)

;;; com.inuoe.jzon
(push-json-reader-alist :com.inuoe.jzon 'com.inuoe.jzon:parse)
