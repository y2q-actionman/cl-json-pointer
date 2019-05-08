(in-package :cl-json-pointer)

(defvar *json-object-flavor* t
  "Default flavor of JSON library currently used.
This value is used for `:flavor' argument of exported functions.
Currently acceptable values are held by `*cl-json-pointer-supported-json-flavors*'

Default is `t', behaves as well as possible without any knowledge about JSON libs.")

;;; Getter family

(defun get-by-json-pointer (obj pointer &key (flavor *json-object-flavor*))
  "Traverses `obj' with `pointer' and returns three values:
the found value (`nil' if not found), a generalized boolean saying the existence of the place pointed by `pointer', and NIL."
  (let ((parsed-ptr (parse-json-pointer pointer)))
    (traverse-by-json-pointer obj flavor parsed-ptr nil)))

(defun exists-p-by-json-pointer (obj pointer &key (flavor *json-object-flavor*))
  "Traverses `obj' with `pointer' and returns the existence of the place pointed by `pointer'."
  (nth-value 1 (get-by-json-pointer obj pointer :flavor flavor)))

;;; Setter family

(defun make-setter-by-json-pointer (obj obj-flavor pointer set-method)
  "Updating functions (`set-by-json-pointer', `delete-by-json-pointer', etc)
calls this for making a setter function."
  (let ((parsed-ptr (parse-json-pointer pointer)))
    (nth-value 2 (traverse-by-json-pointer obj obj-flavor parsed-ptr set-method))))

(defun set-by-json-pointer (obj pointer value &key (flavor *json-object-flavor*))
  "Traverses `obj' with `pointer', sets `value' into the pointed
place, and returns the modified `obj'"
  (funcall (make-setter-by-json-pointer obj flavor pointer :update) value))

(defun add-by-json-pointer (obj pointer value &key (flavor *json-object-flavor*))
  "Works same as `set-by-json-pointer', except this try to make a new
list when setting to lists."
  (funcall (make-setter-by-json-pointer obj flavor pointer :add) value))

(defun delete-by-json-pointer (obj pointer &key (flavor *json-object-flavor*))
  "Traverses `obj' with `pointer', deletes the pointed place, and
returns the modified `obj'"
  (funcall (make-setter-by-json-pointer obj flavor pointer :delete)))

(defun remove-by-json-pointer (obj pointer &key (flavor *json-object-flavor*))
  "Works same as `delete-by-json-pointer', except this try to make a new
list when deleting from lists."
  (funcall (make-setter-by-json-pointer obj flavor pointer :remove)))


(define-setf-expander get-by-json-pointer (obj pointer &key (flavor '*json-object-flavor*) &environment env)
  "A setf expansion for allowing `setf' to `(get-by-json-pointer ...)' forms."
  (multiple-value-bind (o-tmps o-vals o-newval o-setter o-getter)
      (get-setf-expansion obj env)
    (unless (length= 1 o-newval)
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
  set-by-json-pointer
  "Modify macro of `set-by-json-pointer'. This sets results of
`set-by-json-pointer' to the referred place. ")

(define-modify-macro deletef-by-json-pointer (pointer &rest keyargs)
  delete-by-json-pointer
    "Modify macro of `delete-by-json-pointer'. This sets results of
`delete-by-json-pointer' to the referred place. ")
