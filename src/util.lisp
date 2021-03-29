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

(defun clone-and-replace-on-cons (list cons value)
  "Makes a fresh list whose contents is same as LIST except the car of
CONS is replaced with VALUE.  If CONS is not contained in LIST,
returns a new list by appending LIST, (list VALUE) and the cdr of CONS to the LIST."
  (nconc (ldiff list cons) (list value) (cdr cons)))

(defun remove-cons (list cons &optional (count 1))
  "Makes a fresh list whose contents is same as LIST except the CONS
and successive COUNT conses.  If CONS is not contained in LIST,
returns a new list by appending LIST and (nthcdr COUNT CONS)."
  (nconc (ldiff list cons) (nthcdr count cons)))

(defun delete-cons (list cons &optional (count 1))
  "Destructively modifies LIST to exclude the CONS and successive
COUNT conses.  If CONS is not contained in LIST, returns a list by
nconcing LIST and (nthcdr COUNT CONS)."
  (when (or (eq list cons)
            (null list))
    (return-from delete-cons (nthcdr count cons)))
  (loop for c on list
        as cdr-c = (cdr c)
        until (or (eq cdr-c cons)       ; found
                  (null cdr-c)      ; proper list tail (not contained)
                  (not (consp cdr-c)))  ; dotted list tail
        finally
           (setf (cdr c) (nthcdr count cons))
           (return list)))

(defun extend-list (list n &key initial-element)
  "Destructively extends LIST to size N."
  (declare (type integer n))
  (let ((tmp-cons (cons :placeholder list)))
    (declare (dynamic-extent tmp-cons))
    (loop for prev = tmp-cons then c
          for rest-length downfrom n
          for c on list
          while (plusp rest-length)
          finally
             (when (plusp rest-length)
               (setf (cdr prev)
                     (make-list rest-length :initial-element initial-element)))
             (return (cdr tmp-cons)))))

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
  "Makes a new adjustable fill-pointered array having same contents as `array'"
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
