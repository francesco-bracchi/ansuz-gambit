(##namespace
 ("ansuz-kernel#"
  get
  get-if
  get-stream
  get-continuation
  get-backtrack
  fail
  ))

(define-macro+ (get-if t?)
  (let(
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl))
       (ch (gensym 'ch)))
    `(reflect (,st ,sc ,fl)
              (let(
                   (,ch (stream-car ,st)))
                (if (,t? ,ch)
                    (,sc ,ch (stream-cdr ,st) ,fl)
                    (,fl (list 'test-failed ,t? ,ch) ,st ,sc))))))

(define-macro+ (get c0)
  (let(
       (ch (gensym 'ch)))
    `(get-if (lambda (,ch) (eq? ,c0 ,ch)))))

(define-macro+ (get c0)
  (let((ch (gensym 'ch))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
	      (if (eq? (stream-car ,st) ,c0)
		  (,sc ,c0 (stream-cdr ,st) ,fl)
		  (,fl (list 'not ,c0) ,st ,sc)))))
    

(define-macro+ (fail r)
  (let(
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
              (,fl ,r ,st ,sc))))

(define-macro+ (get-stream)
  (let(
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
              (,sc ,st ,st ,fl))))

(define-macro+ (get-continuation)
  (let(
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
              (,sc ,sc ,st ,fl))))

(define-macro+ (get-backtrack)
  (let(
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
              (,sc ,fl ,st ,fl))))
