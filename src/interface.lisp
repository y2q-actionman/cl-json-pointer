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
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (nth-value 2 (traverse-by-json-pointer obj parsed-ptr set-method))))

(defun set-by-json-pointer (obj pointer value)
  (funcall (make-setter-by-json-pointer obj pointer :update) value))

#+ignore
(defsetf get-by-json-pointer (obj pointer) (value)
  `(progn (set-by-json-pointer ,obj ,pointer ,value)
	  ,value))
;;; FIXME: set-by-json-pointer may create a new `obj', so should I set `obj' again?

(defun add-by-json-pointer (obj pointer value)
  (funcall (make-setter-by-json-pointer obj pointer :add) value))

(defun delete-by-json-pointer (obj pointer)
  (funcall (make-setter-by-json-pointer obj pointer :delete)))

(defun remove-by-json-pointer (obj pointer)
  (funcall (make-setter-by-json-pointer obj pointer :remove)))
