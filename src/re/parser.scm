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
   (con (<- s0 (decimal))
	(COMMA)
	(<- s1 (decimal))
	(ret (nfa:repeat p s0 s1)))
   
   (con (<- s0 (decimal))
	(COMMA)
	(ret (nfa:repeat p s0 'inf)))
   
   (con (<- s0 (decimal))
	(ret (nfa:repeat p s0 s0)))
   
   (con (COMMA)
	(<- s1 (decimal))
	(ret (nfa:repeat p 0 s1)))))     

(define-parser (re-times p)
  (con (CURLED_OPEN)
       (<- s0 (re-interval p))
       (CURLED_CLOSE)
       (ret s0)))

(define-parser (re-parenthesis)
  (con (ROUND_OPEN)
       (<- s0 (re-expr))
       (ROUND_CLOSE)
       (ret s0)))

(define-parser (re-range)
  (con (<- s0 (valid-char))
       (MINUS)
       (<- s1 (valid-char))
       (ret (list (cons (char->integer s0) (+ 1 (char->integer s1)))))))

(define-parser (re-range-singleton)
  (con (<- s0 (valid-char))
       (ret (list (cons (char->integer s0) (+ 1 (char->integer s0)))))))

(define-parser (seti)
  (alt (re-range)
       (re-range-singleton)))

(define-parser (re-set)
  (char #\[)
  (<- neg? (alt (char #\~) (ret #f)))
  (<- xs (repeat 1 (seti)))
  (char #\])
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
  (con (DOT)
       (ret (nfa:set `((0 . ,*max*))))))

(define-parser (re-char)
  (con (<- s0 (valid-char))
       (ret (nfa:set
	     (list (cons
		    (char->integer s0)
		    (+ 1 (char->integer s0))))))))

(define-parser (re-atom)
  (alt (re-parenthesis)
       (re-set)
       (re-any)
       (re-char)))

(define-parser (re-factor)
  (con (<- s0 (re-atom))
       (alt (con (QUEST) (ret (nfa:repeat s0 0 1)))
	    (con (PLUS) (ret (nfa:repeat s0  1 'inf)))
	    (con (MULT) (ret (nfa:repeat s0 0 'inf)))
	    (re-times s0)
	    (ret s0))))

(define-parser (re-term)
  (con (<- s0 (re-factor))
       (alt (con (<- s1 (re-term))
		 (ret (nfa:++ s0 s1)))
	    (ret s0))))

(define-parser (re-expr)
  (con (<- s0 (re-term))
       (alt (con (VBAR)
		 (<- s1 (re-expr))
		 (ret (nfa:// s0 s1)))
	    (ret s0))))

(define-parser (re)
  (con (<- s0 (re-expr))
       (eos)
       (ret (nfa->dfa s0))))
