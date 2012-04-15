(##namespace
 ("ansuz-kernel#"
  get
  get-if
  get-stream
  get-continuation
  get-backtrack
  fail
  ))

(define-macrop (get-if t?)
  (let((st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl))
       (ch (gensym 'ch)))
    `(reflect (,st ,sc ,fl)
              (let((,ch (stream-car ,st)))
                (if (,t? ,ch)
                    (,sc ,ch (stream-cdr ,st) ,fl)
                    (,fl (list 'test-failed ,t? ,ch) ,st ,sc))))))

(define-macrop (get c0)
  (let((ch (gensym 'ch))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
	      (if (eq? (stream-car ,st) ,c0)
		  (,sc ,c0 (stream-cdr ,st) ,fl)
		  (,fl (quote (not ,c0)) ,st ,sc)))))

(define-macrop (fail r)
  (let((st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
              (,fl ,r ,st ,sc))))

(define-macrop (get-stream)
  (let((st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
              (,sc ,st ,st ,fl))))

(define-macrop (get-continuation)
  (let((st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
              (,sc ,sc ,st ,fl))))

(define-macrop (get-backtrack)
  (let((st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(reflect (,st ,sc ,fl)
              (,sc ,fl ,st ,fl))))
