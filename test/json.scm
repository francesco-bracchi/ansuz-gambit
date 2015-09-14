(declare (standard-bindings)
         (extended-bindings)
         (not safe)
         (block))

(##namespace ("json.test#"))
(##include "~~/lib/gambit#.scm")

(include "~~ansuz/on-strings#.scm")
(include "~~ansuz/expressions#.scm")
(include "~~ansuz/language#.scm")

(define-parser (spaces)
  (reflect (st sc fl)
           (let loop ((st st))
             (let ((c (stream-car st)))
               (cond
                ((eof-object? c) (sc #t st fl))
                ((char=? c #\space) (loop (stream-cdr st)))
                ((char=? c #\newline) (loop (stream-cdr st)))
                ((char=? c #\tab) (loop (stream-cdr st)))
                (else (sc #t st fl)))))))

(define-parser (json-true)
  #\t #\r #\u #\e
  (ret #t))

(define-parser (json-false)
  #\f #\a #\l #\s #\e
  (ret #f))

(define-parser (json-null)
  #\n #\u #\l #\l
  (ret '()))

(define integer-zero (char->integer #\0))

(define-parser (idigit)
  (<- c (any))
  (let ((d (- (char->integer c) integer-zero)))
    (if (and (>= d 0) (<= d 9))
        (ret d)
        (fail '(not an integer digit)))))

(define-parser (int-more c)
  (alt (cat (<- c1 (idigit))
            (int-more (+ c1 (* 10 c))))
       (ret c)))

(define-parser (intp)
  (<- d (idigit))
  (int-more d))

(define-parser (frac-more m c0)
  (alt (cat (<- c (idigit))
            (frac-more (/ m 10) (+ (* m c) c0)))
       (ret c0)))

(define-parser (frac)
  #\.
  (<- d (idigit))
  (frac-more (/ 1 100) (/ d 10)))

(define (identity x) x)

(define-parser (sign)
  (alt (cat #\+ (ret identity))
       (cat #\- (ret -))
       (ret identity)))
            
(define-parser (pow10)
  (cat (alt #\e #\E)
       (<- s (sign))
       (<- i (intp))
       (ret (s i))))

(define-parser (json-number)
  (<- s (sign))
  (<- ip (intp))
  (<- fp (alt (frac) (ret 0)))
  (<- ex (alt (pow10) (ret 0)))
  (ret (s (if (= 0 fp) ip
              (* (+ ip fp) (expt 10 ex))))))

(define-parser (json-string)
  #\"
  (reflect (st sc fl)
           (let loop ((s '()) (st st))
             (let ((c (stream-car st)))
               (cond
                ((eof-object? c) (fl "unended json string"))
                ((char=? c #\") (sc (list->string (reverse s)) (stream-cdr st) fl))
                ((char=? c #\\)
                 (let((c (stream-car st))
                      (st (stream-cdr st)))
                   (cond
                    ((eof-object? c) (fl "json string incomplete"))
                    ((eq? c #\") (loop (cons #\" s) (stream-cdr st)))
                    ((eq? c #\/) (loop (cons #\/ s) (stream-cdr st)))
                    ((eq? c #\\) (loop (cons #\/ s) (stream-cdr st)))
                    ((eq? c #\b) (loop (cons #\backspace s) (stream-cdr st)))
                    ((eq? c #\f) (loop (cons #\linefeed s) (stream-cdr st)))
                    ((eq? c #\n) (loop (cons #\newline s) (stream-cdr st)))
                    ((eq? c #\r) (loop (cons #\return s) (stream-cdr st)))
                    ((eq? c #\u) (let*((p3 (stream-car st))
                                       (s3 (stream-cdr st))
                                       (p2 (stream-car s3))
                                       (s2 (stream-cdr s3))
                                       (p1 (stream-car s2))
                                       (s1 (stream-cdr s2))
                                       (p0 (stream-car s1))
                                       (s0 (stream-cdr s1))
                                       (int  (+ (arithmetic-shift (- (char->integer p3) integer-zero) 16)
                                                (arithmetic-shift (- (char->integer p2) integer-zero) 8)
                                                (arithmetic-shift (- (char->integer p1) integer-zero) 4)
                                                (arithmetic-shift (- (char->integer p0) integer-zero) 0))))
                                   ;; to be done
                                   (loop (cons (integer->char int) s) s0)))
                    (else (fl "unknow escape char")))))
                (else (loop (cons c s) (stream-cdr st))))))))


(define-parser (json-object-pairs ps)
  (<- k (json-string))
  (spaces)
  #\:
  (spaces)
  (<- v (json-value))
  (spaces)
  (alt (cat #\, (spaces)
            (json-object-pairs (cons (cons k v) ps)))
       (ret (cons (cons k v) ps))))

(define-parser (json-object)
  #\{
  (spaces)
  (<- as (json-object-pairs '()))
  (spaces)
  #\}
  (ret (reverse  as)))

(define-parser (json-array0 vec)
  (<- v (json-value))
  (spaces)
  (alt (cat #\, (spaces) (json-array0 (cons v vec)))
       (ret (cons v vec))))

(define-parser (json-array)
  #\[
  (spaces)
  (<- as (json-array0 '()))
  #\]
  (ret (list->vector (reverse as))))

(define-parser (json-value)
  (alt (json-string)
       (json-object)
       (json-array)
       (json-true)
       (json-false)
       (json-null)
       (json-number)))

(define-parser (json)
  (spaces)
  (<- v (json-value))
  (spaces)
  (eos)
  (ret v))

(define (parse-string s)
  (run (json) s))

(define (dotimes n fun)
  (if (<= n 1) (fun)
      (begin (fun) (dotimes (- n 1) fun))))

(time (pp (dotimes 1000000 (lambda () (parse-string "{ \"x\": true, \"y\": false }")))))

