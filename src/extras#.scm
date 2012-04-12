(##namespace
 ("ansuz-extras#"
  kleene
  times
  upto
  repeat
  repeat-max
  maybe))

(define-macro+ (kleene m)
  (let(
       (mm (gensym 'mm))
       (kl (gensym 'kl))
       (e (gensym 'e))
       (es (gensym 'es)))
    `(reify (,mm (parser-eval ,m))
            (parser-eval
             (letrec(
                     (,kl (parser (,es)
                                  (alt (con (<- ,e (,mm))
					    (,kl (cons ,e ,es)))
				       (ret (reverse ,es))))))
               (,kl '()))))))
  
(define-macro+ (upto n m)
  (let(
       (mm (gensym 'mm))
       (ut (gensym 'ut))
       (e  (gensym 'e))
       (es (gensym 'es))
       (j  (gensym 'j)))
    `(reify (,mm (parser-eval ,m))
            (parser-eval
             (letrec(
                     (,ut (parser (,j ,es)
                                  (if (= ,j 0) (ret (reverse ,es))
                                      (alt (con (<- ,e (,mm))
                                              (,ut (- ,j 1)  (cons ,e ,es)))
                                          (ret (reverse ,es)))))))
               (,ut ,n '()))))))

(define-macro+ (times n m)
  (let(
       (mm (gensym 'm))
       (tm (gensym 'tm))
       (x (gensym 'x))
       (e (gensym 'e))
       (es (gensym'es)))
    `(reify (,mm (parser-eval ,m))
            (parser-eval
             (letrec(
                     (,tm (parser (,x)
                                  (if (= ,x 0) (ret '())
                                      (con (<- ,e (,mm))
                                          (<- ,es (,tm (- ,x 1)))
                                          (ret (cons ,e ,es)))))))
               (,tm ,n))))))
  


(define-macro+ (repeat n m)
  (let(
       (mm (gensym 'mm))
       (h (gensym 'h))
       (t (gensym 't)))
    
    `(parser-eval
      (let(
           (,mm (parser () (parser-eval ,m))))
        (con (<- ,h (times ,n (,mm)))
            (<- ,t (kleene (,mm)))
            (ret (append ,h ,t)))))))


(define-macro+ (repeat-max n m p)
  (let(
       (mm (gensym 'mm))
       (h (gensym 'h))
       (t (gensym 't)))
    `(parser-eval
      (let(
           (,mm (parser () (parser-eval ,p))))
        (con (<- ,h (times ,n (,mm)))
	     (<- ,t (upto (- ,m ,n) (,mm)))
	     (ret (append ,h ,t)))))))

(define-macro+ (maybe m)
  (let(
       (mm (gensym 'm)))
    `(reify (,mm (parser-eval ,m))
            (orelse (parser-eval ,m) (ret '())))))
    

       