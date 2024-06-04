(defun left (n xs)
  (if (null xs) '()
    (if (< (car xs) n)
      (cons (car xs) (left n (cdr xs))) (left n (cdr xs)))))

(defun right (n xs)
  (if (null xs) '()
    (if (>= (car xs) n)
      (cons (car xs) (right n (cdr xs))) (right n (cdr xs)))))

(defun pivot (n xs)
  (list (left n xs) (right n xs)))

(defun quicksort (xs)
  (if (null xs) '()                 
    (let* ((p (car xs)) (l (car (pivot p (cdr xs)))) (r (car (cdr (pivot p (cdr xs))))))
      (append (quicksort l) (list p) (quicksort r)))))