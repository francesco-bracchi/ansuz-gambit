(##namespace
 ("ansuz-language#"
  letp
  letp*
  letrecp
  ifp
  evalp
  
  run
  run-ndet
  parser
  define-parser))

(define-macrop (ifp t? l r)
  (let((ll (gensym 'll))
       (rr (gensym 'rr))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reify (,ll ,l)
            (reify (,rr ,r)
                   (reflect (,st ,sc ,fl)
                            (if ,t?
                                (,ll ,st ,sc ,fl)
                                (,rr ,st ,sc ,fl)))))))

(define-macrop (letp v b)
  (let((bb (gensym 'bb))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reify (,bb ,b)
            (reflect (,st ,sc ,fl)
                     (let ,v (,bb ,st ,sc ,fl))))))

(define-macrop (letp* hs b)
  (let((bb (gensym 'll))
       (rr (gensym 'rr))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reify (,bb ,b)
            (reflect (,st ,sc ,fl)
                     (let* ,hs (,bb ,st ,sc ,fl))))))

(define-macrop (letrecp hs b)
  (let((bb (gensym 'll))
       (rr (gensym 'rr))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reify (,bb ,b)
            (reflect (,st ,sc ,fl)
                     (letrec ,hs (,bb ,st ,sc ,fl))))))

(define-macrop (evalp e)
    (cond
     ((and (pair? e) (eq? (car e) 'cat) (null? (cddr e)))
      `(evalp ,(cadr e)))
     
     ((and (pair? e) (eq? (car e) 'cat) (pair? (cadr e)) (eq? (car (cadr e)) '<-))
      `(bind (,(cadr (cadr e)) (evalp ,(caddr (cadr e)))) (evalp (cat ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) 'cat))
      `(sequence (evalp ,(cadr e)) (evalp (cat ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) 'alt) (null? (cddr e)))
      `(evalp ,(cadr e)))
     
     ((and (pair? e) (eq? (car e) 'alt))
      `(orelse (evalp ,(cadr e)) (evalp (alt ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) 'alt~) (null? (cddr e)))
      `(evalp ,(cadr e)))
     
     ((and (pair? e) (eq? (car e) 'alt~))
      `(orelse* (evalp ,(cadr e)) (evalp (alt~ ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) 'if))
      `(ifp ,(cadr e) (evalp ,(caddr e)) (evalp ,(cadddr e))))

     ((and (pair? e) (eq? (car e) 'let))
      `(letp ,(cadr e) (evalp (cat ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) 'let*))
      `(letp* ,(cadr e) (evalp (cat ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) 'letrec))
      `(letrecp ,(cadr e) (evalp (cat ,@(cddr e)))))

     ((not (pair? e)) `(get ,e))
     (else e)))

(define-macro (run p src #!optional (fail 'error))
  (let((v (gensym 'v))
       (r (gensym 'r))
       (st (gensym 'st))
       (fl (gensym 'fl))
       (sc (gensym 'sc)))
    `(run-monad ,p
                (stream ,src)
                (lambda (,v ,st ,fl) ,v)
                (lambda (,r ,st ,sc) (,fail ,r)))))


(define-macro (run-ndet p src)
  (let((v (gensym 'v))
       (r (gensym 'r))
       (st (gensym 'st))
       (fl (gensym 'fl))
       (sc (gensym 'sc)))
    `(run-monad ,p
                (stream ,src)
                (lambda (,v ,st ,fl) (cons ,v (delay (,fl))))
                (lambda (,r ,st ,sc) '()))))

(define-macro (parser f . b)
  `(lambdap ,f (evalp (cat ,@b))))

(define-macro (define-parser s . b)
  `(define ,(car s) (parser ,(cdr s) ,@b)))
