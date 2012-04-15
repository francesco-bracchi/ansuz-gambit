(declare (standard-bindings)
         (extended-bindings)
         (not safe)
         (block))

(##namespace ("calc#"))
(##include "~~/lib/gambit#.scm")

(include "~~ansuz/on-strings#.scm")
(include "~~ansuz/expressions#.scm")
(include "~~ansuz/language#.scm")

(define-parser (spc)
  (many #\space))

(define idigit
  (let((i0 (char->integer #\0)))
    (parser ()
	    (<- c (digit))
	    (ret (- (char->integer c) i0)))))

(define-parser (int+ c)
  (alt (cat (<- c1 (idigit))
	    (int+ (+ c1 (* 10 c))))
       (ret c)))

(define-parser (int) 
  (<- d (idigit))
  (int+ d))

(define-parser (frac)
  #\.
  (<- d (idigit))
  (frac+ (/ 1 100) (/ d 10)))

(define-parser (frac+ m c0)
  (alt (cat (<- c (idigit))
	    (frac+ (/ m 10) (+ (* m c) c0)))
       (ret c0)))

(define-parser (sign)
  (alt (cat #\+ (ret (lambda (n) n)))
       (cat #\- (ret (lambda (n) (- n))))
       (ret (lambda (n) n))))

(define-parser (pow10)
  (alt #\e #\E)
  (<- s (sign))
  (<- i (int))
  (ret (s i)))

;; NUMBER : 1 1.1 1.1e-10 1.1e+10 +1 -1 1.1e10 ...
(define-parser (num)
  (<- s (sign))
  (<- ip (int))
  (<- fp (alt (frac) (ret 0)))
  (<- ex (alt (pow10) (ret 0)))
  (ret (s (* (+ ip fp) (expt 10 ex)))))

(define-parser (number)
  (<- n (num))
  (eos)
  (ret n))


(define-parser (sum)
  (spc)
  #\+ 
  (ret +))

(define-parser (dif)
  (spc)
  #\- 
  (ret -))

(define-parser (mul)
  (spc)
  #\* 
  (ret *))

(define-parser (div)
  (spc)
  #\/ 
  (ret /))

(define-parser (sqr)
  (spc)
  #\^ (ret (lambda (c) (* c c))))

(define (factorial n)
  (if (< n 1) 1
      (* n (factorial (- n 1)))))

(define-parser (fac)
  (spc)
  #\! (ret factorial))

(define (imaginary n)
  (* +i n))

(define-parser (imag)
  (spc)
  #\i (ret imaginary))

(define table
  (let((prefix `((,dif 1)))
       (infix  `((,sum 1 left)
		 (,dif 1 left)
		 (,mul 2 left)
		 (,div 2 left)))
       (postfix`((,sqr 3)
		 (,fac 4)
		 (,imag 5))))
    (make-operator-table
     prefix
     infix
     postfix)))

(define-parser (par)
  #\(
  (<- e (expr table _term))
  (spc)
  #\)
  (ret e))

(define-parser (_term)
  (spc)
  (alt (num) (par)))

(define-parser (math-exp)
  (<- r (expr table _term))
  (spc)
  (alt (get-if eof-object?) (fail "end expected"))
  ;; (alt (eos) (fail "end expected"))
  (ret r))

(define (calc s)
  (with-exception-catcher
   (lambda (ex) 'fail)
   (lambda () 
     (run (math-exp) s))))

(define (calc-repl)
  (for-each (lambda (s) (display s) (newline))
	    '("this is a simple example for expressions in ansuz"
	      "press ctrl-D to exit"
	      "available commands:"
	      "prefix operators -"
	      "infix operators + - * /"
	      "postfix operators ! ^ i"))
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

(run (math-exp) "10+2")

(calc-repl)
