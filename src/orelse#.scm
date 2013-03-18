;; Orelse
;; there are 2 orelses because there is one that is fully correct that maintains a
;; reference to the branching point even when the left branch matches, and another
;; that if the orelse left branch matches the whole parser returns that.
;; it's clearer by an example.
;; suppose
;; (seq (alt ab a) b)
;; where ab matches string 'ab' , a the string 'a' and b string 'b'
;; the above expression can match
;; 'abb' and 'ab' strings.
;; but if the alt operator is the one that do not mantain the backtrace reference
;; happens that it fails when input is 'ab':
;; the parser first recognize ab, then it expects another b so if simply fails.
;; in the case of orelse* when fail looking for b call backtracking and finds
;; a then b.
;; Why two orelses? The answer is that the not perfectly correct is more efficient
;; because do not create a reference to backtracking closure for each orelse
;; when left branch succeed.
;; In any case incorrect orelse works fine for many cases, and many times when
;; do not work fine we can simply transform it. We can rewrite our example as
;; (alt (cat ab b) (cat a b))
;; that works correctly.
;; For deterministic parsers the incorrect orelse is sufficient to describe any
;; parser.
(##namespace ("ansuz-orelse#" orelse orelse*))

(define-macrop (orelse m n)
  (let((mm (gensym 'm))
       (nn (gensym 'n))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl))
       (rr (gensym 'rr))
       (vv (gensym 'vv))
       (st1 (gensym 'st))
       (sc1 (gensym 'sc))
       (fl1 (gensym 'fl)))
    `(reify (,mm ,m)
            (reify (,nn ,n)
                   (reflect (,st ,sc ,fl)
                            (,mm ,st 
                                 (lambda (,vv ,st1 ,fl1) (,sc ,vv ,st1 ,fl))
                                 ;; (lambda (,vv ,st1 ,zk1 ,fl1) (,sc ,vv ,st1 ,zk1 (if ,zk ,fl ,fl1)))
                                 (lambda (,rr ,st1 ,sc1)
                                   (,nn ,st ,sc ,fl))))))))

;; nondeterministic variant
(define-macrop (orelse* m n)
  (let((mm (gensym 'm))
       (nn (gensym 'n))
       (st (gensym 'st))
       (sc (gensym 'sc))
       (fl (gensym 'fl))
       (rr (gensym 'rr))
       (vv (gensym 'vv))
       (st1 (gensym 'st))
       (sc1 (gensym 'sc))
       (fl1 (gensym 'fl)))
    `(reify (,mm ,m)
            (reify (,nn ,n)
                   (reflect (,st ,sc ,fl)
                            (,mm ,st
                                 ,sc
                                 (lambda (,rr ,st1 ,sc1)
                                   (,nn ,st ,sc ,fl))))))))
