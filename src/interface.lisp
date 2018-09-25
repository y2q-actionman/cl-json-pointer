(in-package :cl-json-pointer)

;;; Getter family

(defun get-by-json-pointer (obj pointer)
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (traverse-by-json-pointer obj parsed-ptr nil)))

(defun exists-p-by-json-pointer (obj pointer)
  (nth-value 1 (get-by-json-pointer obj pointer)))

;;; Setter family

(defun make-setter-by-json-pointer (obj pointer set-method)
  "Updating functions (`set-by-json-pointer', `delete-by-json-pointer', etc)
calls this for making a setter function."
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (nth-value 2 (traverse-by-json-pointer obj parsed-ptr set-method))))

(defun set-by-json-pointer (obj pointer value)
  (funcall (make-setter-by-json-pointer obj pointer :update) value))

(defun add-by-json-pointer (obj pointer value)
  (funcall (make-setter-by-json-pointer obj pointer :add) value))

(defun delete-by-json-pointer (obj pointer)
  (funcall (make-setter-by-json-pointer obj pointer :delete)))

(defun remove-by-json-pointer (obj pointer)
  (funcall (make-setter-by-json-pointer obj pointer :remove)))


(define-setf-expander get-by-json-pointer (obj pointer &environment env)
  (multiple-value-bind (o-tmps o-vals o-newval o-setter o-getter)
      (get-setf-expansion obj env)
    (unless (length= o-newval 1)
      (error "setf to get-by-json-pointer requires the first arg is one value."))
    (with-gensyms (p-tmp store)
      (values (list* p-tmp o-tmps)
	      (list* pointer o-vals)
	      (list store)
	      `(let ((,(first o-newval)	; this binding influences `o-setter'.
		      (set-by-json-pointer ,o-getter ,p-tmp ,store)))
		 ,o-setter
		 ,store)
	      `(get-by-json-pointer ,o-getter ,p-tmp)))))

(define-modify-macro update-by-json-pointer (pointer value)
  set-by-json-pointer)
