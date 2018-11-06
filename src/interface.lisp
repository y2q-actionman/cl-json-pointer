(in-package :cl-json-pointer)

(defvar *json-object-flavor* t
  "Default flavor of JSON library currently used.
This value is used for `:flavor' argument of exported functions.
Currently acceptable values are held by `*cl-json-pointer-supported-json-flavors*'

Default is `t', behaves as well as possible without any knowledge about JSON libs.")

;;; Getter family

(defun get-by-json-pointer (obj pointer &key (flavor *json-object-flavor*))
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (traverse-by-json-pointer obj flavor parsed-ptr nil)))

(defun exists-p-by-json-pointer (obj pointer &key (flavor *json-object-flavor*))
  (nth-value 1 (get-by-json-pointer obj pointer :flavor flavor)))

;;; Setter family

(defun make-setter-by-json-pointer (obj obj-flavor pointer set-method)
  "Updating functions (`set-by-json-pointer', `delete-by-json-pointer', etc)
calls this for making a setter function."
  (let ((parsed-ptr (parse-json-pointer pointer)))
    (nth-value 2 (traverse-by-json-pointer obj obj-flavor parsed-ptr set-method))))

(defun set-by-json-pointer (obj pointer value &key (flavor *json-object-flavor*))
  (funcall (make-setter-by-json-pointer obj flavor pointer :update) value))

(defun add-by-json-pointer (obj pointer value &key (flavor *json-object-flavor*))
  (funcall (make-setter-by-json-pointer obj flavor pointer :add) value))

(defun delete-by-json-pointer (obj pointer &key (flavor *json-object-flavor*))
  (funcall (make-setter-by-json-pointer obj flavor pointer :delete)))

(defun remove-by-json-pointer (obj pointer &key (flavor *json-object-flavor*))
  (funcall (make-setter-by-json-pointer obj flavor pointer :remove)))


(define-setf-expander get-by-json-pointer (obj pointer &key (flavor '*json-object-flavor*) &environment env)
  (multiple-value-bind (o-tmps o-vals o-newval o-setter o-getter)
      (get-setf-expansion obj env)
    (unless (length= o-newval 1)
      (error "setf to get-by-json-pointer requires the first arg is one value."))
    (with-gensyms (p-tmp flavor-tmp store)
      (values (list* p-tmp flavor-tmp o-tmps)
	      (list* pointer flavor o-vals)
	      (list store)
	      `(let ((,(first o-newval)	; this binding influences `o-setter'.
		      (set-by-json-pointer ,o-getter ,p-tmp ,store :flavor ,flavor-tmp)))
		 ,o-setter
		 ,store)
	      `(get-by-json-pointer ,o-getter ,p-tmp :flavor ,flavor-tmp)))))

(define-modify-macro update-by-json-pointer (pointer value &rest keyargs)
  set-by-json-pointer)

(define-modify-macro deletef-by-json-pointer (pointer &rest keyargs)
  delete-by-json-pointer)
