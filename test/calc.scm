(declare (standard-bindings)
         (extended-bindings)
         (not safe)
         (block))


(##namespace ("calc#"))
(##include "~~/lib/gambit#.scm")

(include "~~ansuz/on-strings#.scm")
(include "~~ansuz/expressions#.scm")

(define-parser (spc)
  ;; (many #\space)
  (kleene #\space))

(define idigit
  (let(
       (i0 (char->integer #\0)))
    (parser ()
            (>> (<- c (digit))
                (return (- (char->integer c) i0))))))

(define-parser (int+ c)
  (<> (>> (<- c1 (idigit))
          (int+ (+ c1 (* 10 c))))
      (return c)))

(define-parser (int) 
  (<- d (idigit))
  (int+ d))

(define-parser (frac)
  #\.
  (<- d (idigit))
  (frac+ (/ 1 100) (/ d 10)))

(define-parser (frac+ m c0)
  (<> (>> (<- c (idigit))
          (frac+ (/ m 10) (+ (* m c) c0)))
      (return c0)))

(define-parser (sign)
  (<> (>> #\+ (return (lambda (n) n)))
      (>> #\- (return (lambda (n) (- n))))
      (return (lambda (n) n))))
                      
(define-parser (pow10)
  (<> #\e #\E)
  (<- s (sign))
  (<- i (int))
  (return (s i)))

;; NUMBER : 1 1.1 1.1e-10 1.1e+10 +1 -1 1.1e10 ...
(define-parser (num)
  (<- s (sign))
  (<- ip (int))
  (<- fp (<> (frac) (return 0)))
  (<- ex (<> (pow10) (return 0)))
  (return (s (* (+ ip fp) (expt 10 ex)))))

(define-parser (number)
  (<- n (num))
  (eos)
  (return n))


(define-parser (sum)
  (spc)
  #\+ 
  (return +))

(define-parser (dif)
  (spc)
  #\- 
  (return -))

(define-parser (mul)
  (spc)
  #\* 
  (return *))

(define-parser (div)
  (spc)
  #\/ 
  (return /))

(define-parser (sqr)
  (spc)
  #\^ (return (lambda (c) (* c c))))

(define (factorial n)
  (if (< n 1) 1
      (* n (factorial (- n 1)))))

(define-parser (fac)
  (spc)
  #\! (return factorial))

(define table
  (let((prefix `((,dif 1)))
       (infix  `((,sum 1 left)
		 (,dif 1 left)
		 (,mul 2 left)
		 (,div 2 left)))
       (postfix`((,sqr 3)
		 (,fac 4))))
  (make-operator-table
   prefix
   infix
   postfix)))

(define-parser (par)
  #\(
  (<- e (expr table _term))
  (spc)
  #\)
  (return e))

(define-parser (_term)
  (spc)
  (<> (num) (par)))

(define-parser (math-exp)
  (<- r (expr table _term))
  (spc)
  (<> (eos) (fail "end expected"))
  (return r))

(define (calc s)
  (with-exception-catcher
   (lambda (ex) 'fail)
   (lambda () 
     (run (math-exp) s))))

(define (calc-repl)
  (display "press ctrl-D to exit\n")
  (display "available commands:\n")
  (display "prefix operators -\n")
  (display "infix operators + - * / \n")
  (display "postfix operators ! ^\n")
  (let forever ()
    (display "?")
    (let ((c (read-line)))
      (if (eof-object? c) (newline)
          (let((val (calc c)))
            (if (eq? val 'fail)
                (display "not well formed expression\n")
                (begin
                  (display "=")
                  (display (exact->inexact val))
                  (newline)))
            (forever))))))

(calc-repl)
