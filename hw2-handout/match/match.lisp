(defun equal_func (x y)
  (or (eql x y)
  (and (consp x) (consp y)
  (equal_func (car x) (car y))
  (equal_func (cdr x) (cdr y)))))

(defun match (pattern assertion)
  (cond
    ((and (null pattern) (null assertion)) t)
    ((or (null pattern) (null assertion)) nil)
    ((equal_func (car pattern) '?) (and (not (null assertion)) (match (cdr pattern) (cdr assertion))))
    ((equal_func (car pattern) '!) (or (match pattern (cdr assertion)) (and (not (null assertion)) (match (cdr pattern) (cdr assertion)))))
    (t (and (equal_func (car pattern) (car assertion)) (match (cdr pattern) (cdr assertion))))))
