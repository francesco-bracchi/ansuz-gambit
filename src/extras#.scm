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
(define-macro+ (maybe m)
  (let ((mm (gensym 'm)))
    `(reify (,mm (parser-eval ,m))
            (orelse (parser-eval ,m) (ret '())))))
    
;; 0 to n times
(define-macro+ (many m)
  (let ((mm (gensym 'mm))
	(kl (gensym 'kl))
	(e (gensym 'e))
	(es (gensym 'es)))
    `(reify (,mm (parser-eval ,m))
            (parser-eval
             (letrec ((,kl (parser (,es)
				   (alt (cat (<- ,e (,mm))
					     (,kl (cons ,e ,es)))
					(ret (reverse ,es))))))
               (,kl '()))))))

;; up to n times
(define-macro+ (upto n m)
  (let ((mm (gensym 'mm))
	(ut (gensym 'ut))
	(e  (gensym 'e))
	(es (gensym 'es))
	(j  (gensym 'j)))
    `(reify (,mm (parser-eval ,m))
            (parser-eval
             (letrec ((,ut (parser (,j ,es)
				   (if (= ,j 0) (ret (reverse ,es))
				       (alt (cat (<- ,e (,mm))
						 (,ut (- ,j 1)  (cons ,e ,es)))
					    (ret (reverse ,es)))))))
               (,ut ,n '()))))))

;; exactly n times
(define-macro+ (times n m)
  (let(
       (mm (gensym 'm))
       (tm (gensym 'tm))
       (x (gensym 'x))
       (e (gensym 'e))
       (es (gensym'es)))
    `(reify (,mm (parser-eval ,m))
            (parser-eval
             (letrec ((,tm (parser (,x)
				   (if (= ,x 0) (ret '())
				       (cat (<- ,e (,mm))
					    (<- ,es (,tm (- ,x 1)))
					    (ret (cons ,e ,es)))))))
               (,tm ,n))))))
  
;; at least n times
(define-macro+ (at-least n m)
  (let ((mm (gensym 'mm))
	(h (gensym 'h))
	(t (gensym 't)))
    `(parser-eval
      (let ((,mm (parser () (parser-eval ,m))))
        (cat (<- ,h (times ,n (,mm)))
	     (<- ,t (many (,mm)))
	     (ret (append ,h ,t)))))))

;; between n to m times 
(define-macro+ (at-least/most n m p)
  (let ((mm (gensym 'mm))
	(h (gensym 'h))
	(t (gensym 't)))
    `(parser-eval
      (let ((,mm (parser () (parser-eval ,p))))
        (cat (<- ,h (times ,n (,mm)))
	     (<- ,t (upto (- ,m ,n) (,mm)))
	     (ret (append ,h ,t)))))))

(define-macro+ (many/separated val sep)
  (let((sp (gensym 'sp))
       (vl (gensym 'vl))
       (v (gensym 'v))
       (vs (gensym 'vs)))
    `(reify (,sp ,sep)
            (reify (,vl ,val)
                   (parser-eval
                    (alt (cat (<- ,v (,vl))
			      (<- ,vs (many (cat (,sp) (,vl))))
			      (ret (cons ,v ,vs)))
                        (ret '())))))))