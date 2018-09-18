(in-package :cl-json-pointer)

(defun get-by-json-pointer (obj pointer)
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (traverse-by-json-pointer obj parsed-ptr nil nil)))

(defun exists-p-by-json-pointer (obj pointer)
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (nth-value 1 (traverse-by-json-pointer obj parsed-ptr nil nil))))

(defun set-by-json-pointer (obj pointer value)
  (let* ((parsed-ptr
	  (parse-json-pointer pointer))
	 (setter
	  (nth-value 2 (traverse-by-json-pointer obj parsed-ptr t nil))))
    (funcall setter value)))

(defsetf get-by-json-pointer (obj pointer) (value)
  `(progn (set-by-json-pointer ,obj ,pointer ,value)
	  ,value))

(defun delete-by-json-pointer (obj pointer)
  (let* ((parsed-ptr
	  (parse-json-pointer pointer))
	 (deleter
	  (nth-value 3 (traverse-by-json-pointer obj parsed-ptr nil t))))
    (funcall deleter)))
