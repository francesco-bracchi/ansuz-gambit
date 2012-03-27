(##namespace
 ("ansuz-reflect#"
  with-state
  reflect
  reify
  run-monad))

(define-macro (with-state x c)
  (append c x))

(define-macro (reflect v b . x)
  (let((vx (map cons v x)))
    (let replace ((b b))
      (cond
       ((assoc b vx) => cdr)
       ((pair? b) (cons (replace (car b))
			(replace (cdr b))))
       (else b)))))

(define-macro (reify vm val . x)
  (let((v (car vm))
       (m (cadr vm)))
    `(with-state
      ,x
      ,(let replace ((val val))
	 (cond
	  ((not (pair? val)) val)
	  ((eq? (car val) v) (append m (replace (cdr val))))
	  (else (cons (replace (car val)) 
		      (replace (cdr val)))))))))

(define-macro (run-monad p . x)
  `(with-state ,x ,p))
