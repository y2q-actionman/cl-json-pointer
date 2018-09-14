(in-package :cl-json-pointer)

(defun get-by-json-pointer (obj pointer)
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (traverse-by-json-pointer obj parsed-ptr)))

(defun exists-p-by-json-pointer (obj pointer)
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (nth-value 1 (traverse-by-json-pointer obj parsed-ptr))))

(defun set-by-json-pointer (obj pointer value)
  (let* ((parsed-ptr
	  (parse-json-pointer pointer))
	 (setter
	  (nth-value 2 (traverse-by-json-pointer obj parsed-ptr))))
    (funcall setter value)))

(defsetf get-by-json-pointer (obj pointer) (value)
  `(progn (set-by-json-pointer ,obj ,pointer ,value)
	  ,value))
