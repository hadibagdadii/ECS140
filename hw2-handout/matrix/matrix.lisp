(defun matrix-add (mat1 mat2)
  (if (and mat1 mat2)
    (let ((row1 (car mat1)) (row2 (car mat2)))
      (if (and row1 row2)
        (cons (mapcar #'+ row1 row2) (matrix-add (cdr mat1) (cdr mat2))) nil)) nil))

(defun matrix-transpose (mat)
  (apply #'mapcar #'list mat))

(defun matrix-multiply (mat1 mat2)
  (let ((mat2-transposed (apply #'mapcar #'list mat2)))
    (mapcar #'(lambda (row1) (mapcar #'(lambda (col2) (apply #'+ (mapcar #'* row1 col2))) mat2-transposed)) mat1)))