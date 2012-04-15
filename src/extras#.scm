(##namespace
 ("ansuz-extras#"
  maybe
  many
  times
  upto
  at-least
  at-least/most
  many/separated))

;; 0 or 1 times
(define-macrop (maybe m)
  (let ((mm (gensym 'm)))
    `(reify (,mm (evalp ,m))
            (orelse (evalp ,m) (ret '())))))

;; 0 to n times
(define-macrop (many m)
  (let ((mm (gensym 'mm))
	(kl (gensym 'kl))
	(e (gensym 'e))
	(es (gensym 'es)))
    `(reify (,mm (evalp ,m))
            (evalp
             (letrec ((,kl (parser (,es)
				   (alt (cat (<- ,e (,mm))
					     (,kl (cons ,e ,es)))
					(ret (reverse ,es))))))
               (,kl '()))))))

;; up to n times
(define-macrop (upto n m)
  (let ((mm (gensym 'mm))
	(ut (gensym 'ut))
	(e  (gensym 'e))
	(es (gensym 'es))
	(j  (gensym 'j)))
    `(reify (,mm (evalp ,m))
            (evalp
             (letrec ((,ut (parser (,j ,es)
				   (if (= ,j 0) (ret (reverse ,es))
				       (alt (cat (<- ,e (,mm))
						 (,ut (- ,j 1)  (cons ,e ,es)))
					    (ret (reverse ,es)))))))
               (,ut ,n '()))))))

;; exactly n times
(define-macrop (times n m)
  (let(
       (mm (gensym 'm))
       (tm (gensym 'tm))
       (x (gensym 'x))
       (e (gensym 'e))
       (es (gensym'es)))
    `(reify (,mm (evalp ,m))
            (evalp
             (letrec ((,tm (parser (,x)
				   (if (= ,x 0) (ret '())
				       (cat (<- ,e (,mm))
					    (<- ,es (,tm (- ,x 1)))
					    (ret (cons ,e ,es)))))))
               (,tm ,n))))))

;; at least n times
(define-macrop (at-least n m)
  (let ((mm (gensym 'mm))
	(h (gensym 'h))
	(t (gensym 't)))
    `(evalp
      (let ((,mm (parser () (evalp ,m))))
        (cat (<- ,h (times ,n (,mm)))
	     (<- ,t (many (,mm)))
	     (ret (append ,h ,t)))))))

;; between n to m times 
(define-macrop (at-least/most n m p)
  (let ((mm (gensym 'mm))
	(h (gensym 'h))
	(t (gensym 't)))
    `(evalp
      (let ((,mm (parser () (evalp ,p))))
        (cat (<- ,h (times ,n (,mm)))
	     (<- ,t (upto (- ,m ,n) (,mm)))
	     (ret (append ,h ,t)))))))

(define-macrop (many/separated val sep)
  (let((sp (gensym 'sp))
       (vl (gensym 'vl))
       (v (gensym 'v))
       (vs (gensym 'vs)))
    `(reify (,sp ,sep)
            (reify (,vl ,val)
                   (evalp
                    (alt (cat (<- ,v (,vl))
			      (<- ,vs (many (cat (,sp) (,vl))))
			      (ret (cons ,v ,vs)))
			 (ret '())))))))
