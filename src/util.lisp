(in-package :cl-json-pointer)

(defun alist-like-p (list)
  (every #'consp list))

(defun plist-like-p (list)
  (loop for (k nil) on list by #'cddr
     always (symbolp k)))

(defun find-previous-cons (list cons &optional copy-head-p) ; used by list setters.
  "Finds the previous cons of the `cons' in the `list'.
If `copy-head-p' is true, makes a partial copy of the `list' between the
head and the previous cons of the passed cons."
  (loop for c on list
     when copy-head-p
     collect (car c) into copy-head
     until (eq (cdr c) cons)
     finally
       (return (values c copy-head))))

(defun make-replaced-list-on-cons (list cons value)
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

(defun compare-string-by-readtable-case (a b &optional (case (readtable-case *readtable*)))
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
