(in-package :cl-json-pointer)

(defun alist-like-p (list)
  (every #'consp list))

(defun plist-like-p (list)
  ;; I think there is no way to define a good `plist-like-p', because plist
  ;; does not restricted on its keys. Whether its keys are compared by `eq'
  ;; (http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm),
  ;; I think I should not assume the keys are always a symbol.
  (loop for (k nil) on list by #'cddr
     always (symbolp k)))

(defun find-previous-cons (list cons &optional copy-head-p) ; used by list setters.
  "Finds the previous cons of the `cons' in the `list'.
If `copy-head-p' is true, makes a partial copy of the `list' between the
head and the previous cons of the passed cons."
  (declare (type list list) (type cons cons))
  (loop for c on list
     when copy-head-p
     collect (car c) into copy-head
     until (eq (cdr c) cons)
     finally
       (return (values c copy-head))))

(defun clone-and-replace-on-cons (list cons value)
  (multiple-value-bind (prev-cons heads)
      (find-previous-cons list cons t)
    (declare (ignore prev-cons))
    (nconc heads (list value) (cdr cons))))

(defun remove-cons (list cons &optional (count 1))
  (multiple-value-bind (prev-cons heads)
      (find-previous-cons list cons t)
    (declare (ignore prev-cons))
    (nconc heads (nthcdr count cons))))

(defun delete-cons (list cons &optional (count 1))
  (if (eq list cons)
      (nthcdr count list)
      (let ((prev-cons (find-previous-cons list cons)))
	(setf (cdr prev-cons) (nthcdr count cons))
	list)))

(defun extend-list (list n)
  "Destructively extends `list' to size `n'."
  ;; TODO: should be more efficient..
  (let ((len (length list)))
    (if (<= n len)
	list
	(nconc list (make-list (- n len))))))

(defun compare-string-by-readtable-case (a b &key (case (readtable-case *readtable*)))
  ;; TODO: should I use `ignore-errors' for alist (or plist) ?
  (ecase case
    ((:upcase :downcase) (string-equal a b))
    ((:preserve :invert) (string= a b))))

(defmacro thunk-lambda (&body form)
  "Used for making thunks."
  (with-gensyms (_)
    `(lambda (&rest ,_)
       (declare (ignore ,_))
       ,@form)))
