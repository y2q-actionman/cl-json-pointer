(in-package :cl-json-pointer)

;;; Lists

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

;;; Arrays

(defun array-try-push (array x)
  (let ((adjustable? (adjustable-array-p array))
	(has-fill-pointer? (array-has-fill-pointer-p array)))
    (if has-fill-pointer?
	(if adjustable?
	    (vector-push-extend x array)
	    (vector-push x array)) ; uses `vector-push' result as condition.
	nil)))

(defun extend-array (array new-length fill-pointer)
  "Makes a new adjutable fill-pointered array having same contents as `array'"
  (if (adjustable-array-p array)
      (adjust-array array new-length :initial-element nil
		    :fill-pointer fill-pointer)
      (let ((new-array (make-array new-length :adjustable t :initial-element nil
				   :fill-pointer fill-pointer)))
	(replace new-array array)
	new-array)))

;;; Others

(defun compare-string-by-readtable-case (a b &key (case (readtable-case *readtable*)))
  ;; TODO: For removing this, I require 'try-intern' like one!
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
