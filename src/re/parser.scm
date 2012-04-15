(##namespace ("ansuz-re-parser#"))

(##include "~~/lib/gambit#.scm")

(include "../sources/string#.scm")
(include "../char-stream-parser#.scm")
(include "../extras#.scm")

(include "sets#.scm")
(include "fsm#.scm")

(define *-not-valid-*
  (cons #!eof (string->list "|?+*~.-\\(){}[]")))

(define-parser (VBAR)    #\|)
(define-parser (QUEST)   #\?)
(define-parser (PLUS)    #\+)
(define-parser (MULT)    #\*)
(define-parser (TILDE)   #\~)
(define-parser (DOT)     #\.)
(define-parser (MINUS)   #\-)
(define-parser (COMMA)   #\,)
(define-parser (ESCAPE)  #\\)

(define-parser (ROUND_OPEN)   #\()
(define-parser (ROUND_CLOSE)  #\))
(define-parser (CURLED_OPEN)  #\{)
(define-parser (CURLED_CLOSE) #\})

(define-macro (i0)
  (char->integer #\0))

(define-parser (decimal)
  (reflect (st sc fl)
           (let(
                (c (stream-car st)))
             (if (char-numeric? c)
                 (let loop ((st (stream-cdr st))
                            (k (- (char->integer c) (i0))))
                   (let((c (stream-car st)))
                     (if (char-numeric? c)
                         (loop (stream-cdr st)
                               (+ (* k 10) (- (char->integer c) (i0))))
                         (sc k st fl))))
                 (fl "not a decimal" st sc)))))

(define-parser (valid-char)
  (reflect (st sc fl)
           (let(
                (c (stream-car st)))
             (cond
              ((eof-object? c) (fl "end of object reached" st sc))
              ((char=? c #\\)
               (let*((st (stream-cdr st))
                     (c1 (stream-car st)))
                 (if (eof-object? c)
                     (fl "end of object reached" st sc)
                     (sc c1 (stream-cdr st) fl))))
              
              ((memq c *-not-valid-*)
               (fl "not a valid char" st sc))
              (else
               (sc c (stream-cdr st) fl))))))

(define-parser (re-interval p)
  (alt
   (cat (<- s0 (decimal))
	#\,
	(<- s1 (decimal))
	(ret (nfa:repeat p s0 s1)))
   
   (cat (<- s0 (decimal))
	#\,
	(ret (nfa:repeat p s0 'inf)))
   
   (cat (<- s0 (decimal))
	(ret (nfa:repeat p s0 s0)))
   
   (cat #\,
	(<- s1 (decimal))
	(ret (nfa:repeat p 0 s1)))))

(define-parser (re-times p)
  #\{
  (<- s0 (re-interval p))
  #\}
  (ret s0))

(define-parser (re-parenthesis)
  #\(
  (<- s0 (re-expr))
  #\)
  (ret s0)))

(define-parser (re-range)
  (<- s0 (valid-char))
  #\-
  (<- s1 (valid-char))
  (ret (list (cons (char->integer s0) (+ 1 (char->integer s1))))))

(define-parser (re-range-singleton)
  (<- s0 (valid-char))
  (ret (list (cons (char->integer s0) (+ 1 (char->integer s0))))))

(define-parser (seti)
  (alt (re-range)
       (re-range-singleton)))

(define-parser (re-set)
  #\[
  (<- neg? (alt #\~ (ret #f)))
  (<- xs (repeat 1 (seti)))
  #\]
  ;;(ret (pp (set-complement (set-union+ xs))))
  (ret (nfa:set (if neg? (set-complement (set-union+ xs))
		    (set-union+ xs)))))

(define (set-union+ xs)
  (if (null? xs) '()
      (let set-union+ ((x (car xs))
                       (xs (cdr xs)))
        (if (null? xs) x
            (set-union+ (set-union x (car xs)) (cdr xs))))))

(define-parser (re-any)
  #\.
  (ret (nfa:set `((0 . ,*max*)))))

(define-parser (re-char)
  (<- s0 (valid-char))
  (ret (nfa:set
	(list (cons
	       (char->integer s0)
	       (+ 1 (char->integer s0)))))))

(define-parser (re-atom)
  (alt (re-parenthesis)
       (re-set)
       (re-any)
       (re-char)))

(define-parser (re-factor)
  (<- s0 (re-atom))
  (alt (cat #\? (ret (nfa:repeat s0 0 1)))
       (cat #\+ (ret (nfa:repeat s0  1 'inf)))
       (cat #\* (ret (nfa:repeat s0 0 'inf)))
       (re-times s0)
       (ret s0))))

(define-parser (re-term)
  (<- s0 (re-factor))
  (alt (cat (<- s1 (re-term)) (ret (nfa:++ s0 s1)))
       (ret s0))))

(define-parser (re-expr)
  (<- s0 (re-term))
  (alt (cat #\| 
	    (<- s1 (re-expr))
	    (ret (nfa:// s0 s1)))
       (ret s0)))

(define-parser (re)
  (<- s0 (re-expr))
  (eos)
  (ret (nfa->dfa s0)))
