;; given a finite state machine generates a scheme expression
;; that is a valid parser in the sense of futhark/ansuz/

;; TODO, what about eof reached?
;; modify to handle this case
(##namespace ("ansuz-re-cgen#"))
(##include "~~/lib/gambit#.scm")

(include "parser#.scm")
(include "fsm#.scm")
(include "sets#.scm")

;; test every element in list l with function t?
;; if the result is true it added to the result.
;; example
;; (filter odd? '(1 2 3 4 5 6 7)) -> '(1 3 5 7)
(define (filter t? l)
  (cond
   ((null? l) '())
   ((t? (car l)) (cons (car l) (filter t? (cdr l))))
   (else (filter t? (cdr l)))))

;; initial buffer size
(define initial-size 64)

;; takes a fsm
;; that is supposed to be a deterministic finite automata
(define (fsm->code fsm)
  
  ;; transform a set in the for of list of intervals
  ;; in a predicate suitable to be part of a cond clause
  (define (set->predicate s)
    `(or ,@(map interval->predicate s)))

  ;; transform an interval in the form (<lower> . <upper>)
  ;; where lower and upper are numbers, not chars
  ;; in a predicate suitable to be part of a cond clause
  (define (interval->predicate i)
    (if (= (+ 1 (car i)) (cdr i))
        `(char=? c ,(integer->char (car i)))
        `(and
          ,@(append
             (if (> (car i) 0)
                 `((char>=? c ,(integer->char (car i))))
                 '())
             (if (< (car i) *max*)
                 `((char<? c ,(integer->char (cdr i)))))))))

  ;; tranform a transition in a clause for a cond expression
  (define (transition->branch t)
    (let(
         (term? (memq (car t) (fsm-final-states fsm))))
      `(,(set->predicate (cadr t))
        (string-set! buf wpos c)
        (,(caddr t)
         (if (= lim wpos) (string-append buf (make-string (+ 1 lim))) buf) ;; buf
         (if (= lim wpos) (+ 1 (* 2 lim)) lim)                             ;; lim
         (stream-cdr wts)                                                  ;; wts
         (+ 1 wpos)                                                        ;; wpos
         ,(if term? 'wts 'fts)                                             ;; fts
         ,(if term? 'wpos 'fpos)))))                                       ;; fpos
  
  ;; generates the cond expression for a particular state of this fsm
  (define (state->condition s)
    `(cond
      ,@(if (memq s (fsm-final-states fsm))
            `(((eof-object? c) (sc (substring buf 0 wpos) wts fl)))
            `(((and (eof-object? c) fpos) (sc (substring buf 0 fpos) fts fl))
              ((eof-object? c) (fl "re failed" fts sc))))
      ,@(map
         transition->branch
         (filter (lambda (t) (eq? (car t) s))
                 (fsm-transition-table fsm)))
      ,@(if (memq s (fsm-final-states fsm))
            `((else (sc (substring buf 0 wpos) wts fl)))
            `((fpos (sc (substring buf 0 fpos) fts fl))
              (else (fl "re-failed" wts sc))))))
;;       (,(if (memq s (fsm-final-states fsm)) #t ,fpos)
;;        (sc (substring buf 0 fpos) fts fl))
;;       (else
;;        (fl ,(string-append "re failed" (symbol->string s))))))

  ;; generates the function from the state
  (define (state->function s)
    `(lambda (buf lim wts wpos fts fpos)
       (let(
            (c (stream-car wts)))
         ,(state->condition s))))
      
  ;; generates a binding between the state and the function 
  (define (state->binding s)
    `(define ,s ,(state->function s)))

  ;; generates code that run the fsm
  (define (fsm->function)
    `(begin
       ;; (declare (not safe) (not inline) (fixnum) (standard-bindings) (extended-bindings))
       ,@(map state->binding (fsm-states fsm))
       (,(fsm-initial-state fsm)
        (make-string ,initial-size) ;; buf
        ,(- initial-size 1)         ;; lim
        st                          ;; wts
        0                           ;; wpos
        #f                          ;; fts
        #f)))                       ;; fpos
        

  ;; MAIN
  ;; the whole wrapped in a reflect expression
  `(reflect (st sc fl)
            ,(fsm->function)))

              
                     
      
               
  