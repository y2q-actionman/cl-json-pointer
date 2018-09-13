(in-package :cl-json-pointer)

(defun get-by-json-pointer (obj pointer)
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (traverse-by-json-pointer obj parsed-ptr)))

(defun exists-by-json-pointer (obj pointer)
  (let ((parsed-ptr
	 (parse-json-pointer pointer)))
    (nth-value 1 (traverse-by-json-pointer obj parsed-ptr))))

(defun set-by-json-pointer (obj pointer value)
  (multiple-value-bind (parsed-ptr lne-count)
      (parse-json-pointer pointer)
    (let ((setter
	   (nth-value 2 (traverse-by-json-pointer obj parsed-ptr
						  :always-make-setter (plusp lne-count)))))
      (funcall setter value))))

(defsetf get-by-json-pointer (obj pointer) (value)
  `(progn (set-by-json-pointer ,obj ,pointer ,value)
	  ,value))
