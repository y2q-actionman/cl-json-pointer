(in-package :cl-json-pointer)

;;; TODO
#+ignore
(defclass parsed-json-pointer ()
  ((token-list :initarg :token-list :initform () :accessor pjp-token-list)))

(define-condition json-pointer-syntax-error (simple-error)
  ())

(define-condition json-pointer-not-found-error (simple-error)
  ())

(defconstant +parse-json-pointer-default-buffer-length+ 16)

(defun parse-json-pointer-from-stream (stream &key (accept-uri-fragment t))
  ;; checks '#'
  (when accept-uri-fragment
    (let ((char0 (read-char stream nil :eof)))
      (case char0
	(#\# (progn))			; '#' accepted
	(:eof (progn))			; do not unread EOF.
	(otherwise (unread-char char0 stream)))))
  ;; checks '/' at the beginning, and consume it here.
  (let ((char0 (read-char stream nil :eof)))
    (case char0
      (:eof ; I think RFC6901 says an empty string should be accepted.
       (return-from parse-json-pointer-from-stream nil))
      (#\/
       (progn))				; ok
      (otherwise
       (error 'json-pointer-syntax-error
	      :format-control "Not started by '/', appeared '~A'"
	      :format-arguments (list char0)))))
  ;; main loop
  (let ((buf (make-array +parse-json-pointer-default-buffer-length+
			 :element-type 'character :adjustable t :fill-pointer 0)))
    (declare (type string buf)
	     (dynamic-extent buf))
    (flet ((make-reference-token ()
	     (prog1 (copy-seq buf)
	       (setf (fill-pointer buf) 0))))
      (loop with parsing-escape-token? of-type boolean = nil
	 for c of-type (or symbol null) = (read-char stream nil :eof)
	 if parsing-escape-token?
	 do (case c
	      (#\0 (vector-push-extend #\~ buf))
	      (#\1 (vector-push-extend #\/ buf))
	      (otherwise
	       (error 'json-pointer-syntax-error
		      :format-control "bad char as escape: ~A"
		      :format-arguments (list c))))
	   (setf parsing-escape-token? nil)
	 else if (eq c :eof)
	 collect (make-reference-token)
	 and do (loop-finish)
	 else if (eq c #\/)
	 collect (make-reference-token)
	 else if (eq c #\~)
	 do (setf parsing-escape-token? t)
	 else
	 do (vector-push-extend c buf)))))
    
(defun parse-json-pointer (string &key (start 0) (end (length string))
				    (accept-uri-fragment t))
  (with-input-from-string (in string :start start :end end)
    (parse-json-pointer-from-stream in :accept-uri-fragment accept-uri-fragment)))


(defconstant +last-nonexistent-element+
  '+last-nonexistent-element+)
;; I think most-negative-fixnum is able to be used, but unsafe.. 

(defun read-reference-token-as-index (reference-token) ; raises parse-error
  (cond ((string= reference-token "-")
	 +last-nonexistent-element+)
	((and (> (length reference-token) 1)
	      (char= (char reference-token 0) #\0))
	 (error 'json-pointer-not-found-error
		:format-control "reference token (~A) should not start with 0 as index"
		:format-arguments (list reference-token)))
	(t
	 (handler-case (parse-integer reference-token)
	   (error ()
	     (error 'json-pointer-not-found-error
		    :format-control "reference token (~A) cannot be read as index"
		    :format-arguments (list reference-token)))))))

(defun alist-like-p (list)
  (every #'consp list))

(defvar *traverse-consider-plist* nil
  ;; I think there is no way to define `plist-like-p', because plist
  ;; does not restricted. Whether its keys are compared by `eq'
  ;; (http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm),
  ;; I think I should not assume the keys are always a symbol.
  "If this is T, cl-json-pointer considers plists at traversaling")

(defun traverse-by-reference-token (obj rtoken)
  (typecase obj
    (list
     ;; As an alist
     (when (alist-like-p obj)
       (ignore-errors
	 (alexandria:when-let ((entry (assoc rtoken obj :test #'string=)))
	   (return-from traverse-by-reference-token
	     (cdr entry)))))
     ;; As a plist (required?)
     (when *traverse-consider-plist*
       (ignore-errors
	 (alexandria:when-let ((value (getf obj rtoken)))
	   (return-from traverse-by-reference-token
	     value))))
     ;; As a (ordinal) list
     (let ((obj-len (length obj))
	   (index (read-reference-token-as-index rtoken)))
       (cond ((eq index +last-nonexistent-element+) ; see below
	      (assert nil () "under implementation"))
	     ((or (< index 0) (<= obj-len index))
	      (error 'json-pointer-not-found-error
		     :format-control "Index ~A (pointer ~A) is out-of-index from ~A"
		     :format-arguments (list index rtoken obj)))
	     (t
	      (return-from traverse-by-reference-token
		(nth index obj)))))
     ;; Unfortunately..
     (error 'json-pointer-not-found-error
	    :format-control "obj ~A does not have '~A' member"
	    :format-arguments (list obj rtoken)))
    (standard-object
     ;; cl-json:fluid-object can be treated here.
     ;; TODO: get slot list by MOP.
     ;; TODO: support structure-object?
     ;; TODO: support condition-object?
     (assert nil () "under implementation"))
    ;; ???
    ;; (hash-table
    ;;  (progn))
    (array
     (let ((obj-len (length obj))
	   (index (read-reference-token-as-index rtoken)))
       (cond ((eq index +last-nonexistent-element+)
	      ;; TODO: support single '-' as the non-existent last element.
	      ;; - make a closure as a reference?
	      (assert nil () "under implementation"))
	     ((or (< index 0) (<= obj-len index))
	      (error 'json-pointer-not-found-error
		     :format-control "Index ~A (pointer ~A) is out-of-index from ~A"
		     :format-arguments (list index rtoken obj)))
	     (t
	      (aref obj index)))))
    (t
     (error 'json-pointer-not-found-error
	    :format-control "obj ~A is not an array or an object (pointer is ~A)"
	    :format-arguments (list obj rtoken)))))

(defun traverse-json (parsed-json parsed-pointer)
  (loop for obj = parsed-json then (traverse-by-reference-token obj rtoken)
     for (rtoken . rest-ptr) on parsed-pointer
     ;; do (format t "~&obj ~S~% rtoken ~S, rest-ptr ~S~%" obj rtoken rest-ptr)
     finally (return obj)))

(defgeneric get-by-json-pointer (json-obj json-ptr))
(defgeneric set-by-json-pointer (json-obj json-ptr))
