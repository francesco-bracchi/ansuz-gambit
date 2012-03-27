(##namespace ("ansuz-re-fgen#"))

(##include "~~/lib/gambit#.scm")
(include "parser#.scm")
(include "fsm#.scm")
(include "sets#.scm")


(define (filter t? l)
  (cond
   ((null? l) '())
   ((t? (car l)) (cons (car l) (filter t? (cdr l))))
   (else (filter t? (cdr l)))))


(define (fsm->code fsm)
  (define (set->predicate s)
    `(or ,@(map interval->predicate s)))

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
        (,(caddr t)
         (+ 1 wpos)
         ,(if term? 'wpos 'fpos)))))
  
  (define (state->condition s)
    `(if (>= wpos strlen) ,(if (memq s (fsm-final-states fsm)) 'wpos 'fpos)
         (let(
              (c (string-ref str wpos)))
           (cond
            ,@(map
               transition->branch
               (filter (lambda (t) (eq? (car t) s))
                       (fsm-transition-table fsm)))
            ,@(if (memq s (fsm-final-states fsm))
                  `((else wpos))
                  `((else fpos)))))))
  

  (define (state->function s)
    `(lambda (wpos fpos)
       ,(state->condition s)))
  
  `(lambda (str p0)
     (declare (not safe) (fixnum) (block))
     (letrec(
             (strlen (string-length str))
             ,@(map (lambda (s)
                      `(,s (lambda (wpos fpos) ,(state->condition s))))
                    (fsm-states fsm)))
       (,(fsm-initial-state fsm) p0 #f))))