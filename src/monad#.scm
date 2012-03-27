(##namespace ("ansuz-monad#"
              lambda+
              define-macro+
              return
              bind
              sequence))

(define-macro (lambda+ f b)
  (let(
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))

    `(lambda ,(append f (list st sc fl))
       (with-state (,st ,sc ,fl) ,b))))

(define-macro (define+ a b)
  `(define ,(car a) (lambda+ ,(cdr a) ,b)))

(define-macro (define-macro+ h b)
  (let(
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(define-macro ,(append h (list st sc fl))
       (list 'with-state (list ,st ,sc ,fl) ,b))))

(define-macro+ (return v)
  (let(
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl) (,sc ,v ,st ,fl))))

(define-macro+ (bind p n)
  (let(
       (v (car p))
       (m (cadr p))
       (mm (gensym 'mm))
       (nn (gensym 'nn))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl))
       (st1 (gensym 'st))
       (fl1 (gensym 'fl)))
    `(reify (,mm ,m)
            (reify (,nn ,n)
                   (reflect (,st ,sc ,fl)
                            (,mm ,st (lambda (,v ,st1 ,fl1) (,nn ,st1 ,sc ,fl1)) ,fl))))))

(define-macro+ (sequence a b)
  (let(
       (v (gensym 'v)))
    `(bind (,v ,a) ,b)))