(##namespace
 ("ansuz-language#"
  let+
  let*+
  letrec+
  if+
  parser-eval
  
  run
  run-ndet
  parser
  define-parser))

(define-macro+ (if+ t? l r)
  (let(
       (ll (gensym 'll))
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

(define-macro+ (let+ v b)
  (let(
       (bb (gensym 'bb))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reify (,bb ,b)
            (reflect (,st ,sc ,fl)
                     (let ,v (,bb ,st ,sc ,fl))))))

(define-macro+ (let*+ hs b)
  (let(
       (bb (gensym 'll))
       (rr (gensym 'rr))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reify (,bb ,b)
            (reflect (,st ,sc ,fl)
                     (let* ,hs (,bb ,st ,sc ,fl))))))

(define-macro+ (letrec+ hs b)
  (let(
       (bb (gensym 'll))
       (rr (gensym 'rr))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reify (,bb ,b)
            (reflect (,st ,sc ,fl)
                     (letrec ,hs (,bb ,st ,sc ,fl))))))

(define-macro+ (parser-eval e)
    (cond
     ((and (pair? e) (eq? (car e) '>>) (null? (cddr e)))
      `(parser-eval ,(cadr e)))
     
     ((and (pair? e) (eq? (car e) '>>) (pair? (cadr e)) (eq? (car (cadr e)) '<-))
      `(bind (,(cadr (cadr e)) (parser-eval ,(caddr (cadr e)))) (parser-eval (>> ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) '>>))
      `(sequence (parser-eval ,(cadr e)) (parser-eval (>> ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) '<>) (null? (cddr e)))
      `(parser-eval ,(cadr e)))
     
     ((and (pair? e) (eq? (car e) '<>))
      `(orelse (parser-eval ,(cadr e)) (parser-eval (<> ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) '<*>) (null? (cddr e)))
      `(parser-eval ,(cadr e)))
     
     ((and (pair? e) (eq? (car e) '<*>))
      `(orelse* (parser-eval ,(cadr e)) (parser-eval (<*> ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) 'if))
      `(if+ ,(cadr e) (parser-eval ,(caddr e)) (parser-eval ,(cadddr e))))

     ((and (pair? e) (eq? (car e) 'let))
      `(let+ ,(cadr e) (parser-eval (>> ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) 'let*))
      `(let*+ ,(cadr e) (parser-eval (>> ,@(cddr e)))))
     
     ((and (pair? e) (eq? (car e) 'letrec))
      `(letrec+ ,(cadr e) (parser-eval (>> ,@(cddr e)))))

     ((not (pair? e)) `(get ,e))
     (else e)))

;; (define-macro (run p s #!optional (fail 'raise))
;;   (let(
;;        (src (gensym 'src))
;;        (head (gensym 'head))
;;        (tail (gensym 'tail))
;;        (row (gensym 'row))
;;        (col (gensym 'col))
;;        (pos (gensym 'pos))
;;        (datum (gensym 'datum))
;;        (v (gensym 'v))
;;        (fl (gensym 'fl)))
;;     `(let*(
;;            (,src (->source ,s))
;;            (,head (char-source-head ,src))
;;            (,tail (char-source-tail ,src))
;;            (,row (char-source-row ,src))
;;            (,col (char-source-column ,src))
;;            (,pos (char-source-position ,src))
;;            (,datum (char-source-datum ,src)))
;;        (run-monad ,p ,head ,tail ,row ,col ,pos ,datum (lambda (,v ,datum ,fl) ,v) ,fail))))

;; (define-macro (run-ndet p s)
;;   (let(
;;        (src (gensym 'src))
;;        (head (gensym 'head))
;;        (tail (gensym 'tail))
;;        (row (gensym 'row))
;;        (col (gensym 'col))
;;        (pos (gensym 'pos))
;;        (datum (gensym 'datum))
;;        (v (gensym 'v))
;;        (fl (gensym 'fl))
;;        (r (gensym 'r)))
;;     `(let*(
;;            (,src ,s)
;;            (head (char-source-head ,src))
;;            (tail (char-source-tail ,src))
;;            (row (char-source-row ,src))
;;            (col (char-source-column ,src))
;;            (pos (char-source-position ,src))
;;            (datum (char-source-datum ,src)))
;;        (run-monad ,p ,head ,tail ,row ,col ,pos ,datum (lambda (,v ,datum ,fl) (cons ,v (delay (,fl 'ndet)))) (lambda (,r) '())))))

               

;; (define-macro (parser f . b)
;;   `(lambda+ ,f (parser-eval (>> ,@b))))

;; (define-macro (define-parser s . b)
;;   `(define ,(car s) (parser ,(cdr s) ,@b)))


(define-macro (run p src #!optional (fail 'error))
  (let(
       (v (gensym 'v))
       (r (gensym 'r))
       (st (gensym 'st))
       (fl (gensym 'fl))
       (sc (gensym 'sc)))
    `(run-monad ,p
                (stream ,src)
                (lambda (,v ,st ,fl) ,v)
                (lambda (,r ,st ,sc) (,fail ,r)))))


(define-macro (run-ndet p src)
  (let(
       (v (gensym 'v))
       (r (gensym 'r))
       (st (gensym 'st))
       (fl (gensym 'fl))
       (sc (gensym 'sc)))
    `(run-monad ,p
                (stream ,src)
                (lambda (,v ,st ,fl) (cons ,v (delay (,fl))))
                (lambda (,r ,st ,sc) '()))))
  

(define-macro (parser f . b)
  `(lambda+ ,f (parser-eval (>> ,@b))))

(define-macro (define-parser s . b)
  `(define ,(car s) (parser ,(cdr s) ,@b)))