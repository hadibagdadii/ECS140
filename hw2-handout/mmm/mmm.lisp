(defun length_func (l)
  (if (null l) 0
    (+ 1 (length_func (cdr l)))))

(defun min (lst)
  (if (null (cdr lst)) (car lst)
    (let ((rest-min (min (cdr lst))))
      (if (< (car lst) rest-min) (car lst) rest-min))))

(defun mean (xs)
  (let* ((sum (apply #'+ xs)) (xs-length (length_func xs)) (result (/ sum xs-length))) result))

(defun max (lst)
  (if (null (cdr lst)) (car lst)
    (let ((rest-max (max(cdr lst))))
      (if (> (car lst) rest-max) (car lst) rest-max))))

(defun min-mean-max (xs)
  (cond
    ((null xs) nil)
    ((= (length_func xs) 1) (list (car xs) (car xs) (car xs)))
    (t
      (let* ((min-value (min xs)) (max-value (max xs)) (mean (mean xs)))
        (list min-value mean max-value)))))