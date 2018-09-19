(in-package :cl-json-pointer)

(defun get-by-json-pointer (obj pointer)
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (traverse-by-json-pointer obj parsed-ptr nil)))

(defun exists-p-by-json-pointer (obj pointer)
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (nth-value 1 (traverse-by-json-pointer obj parsed-ptr nil))))

(defun set-by-json-pointer (obj pointer value)
  (let* ((parsed-ptr
	  (parse-json-pointer pointer))
	 (setter
	  (nth-value 2 (traverse-by-json-pointer obj parsed-ptr t))))
    (funcall setter value)))

#+ignore
(defsetf get-by-json-pointer (obj pointer) (value)
  `(progn (set-by-json-pointer ,obj ,pointer ,value)
	  ,value))
;;; FIXME: set-by-json-pointer may create a new `obj', so should I set `obj' again?

(defun delete-by-json-pointer (obj pointer)
  (set-by-json-pointer obj pointer +delete-request+))
