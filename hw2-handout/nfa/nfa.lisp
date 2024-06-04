(defun equal_func (x y)
  (or (eql x y)
  (and (consp x) (consp y)
  (equal_func (car x) (car y))
  (equal_func (cdr x) (cdr y)))))

(defun reachable (transitions start final input)
  ; Check if final state is reachable from start state via transitions and input.
  (cond
    ((and (not (equal_func start final)) (null input)) nil) ; No input and start not equal to final
    ((null input) t)                                        ; No input and start equals final
    (t (let ((next (funcall transitions start (car input))))
        (if (null next) nil
            (let ((results (mapcar (lambda (n) (reachable transitions n final (cdr input))) next)))
            (not (zerop (count t results)))))))))