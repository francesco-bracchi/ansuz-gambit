(##namespace ("ansuz-regexp#"))
(##include "~~/lib/gambit#.scm")

(include "re/parser#.scm")
(include "re/fsm#.scm")
(include "re/sets#.scm")

(include "on-strings#.scm")

(declare (fixnum)
         (not safe)
         (block))

(define (filter t? l)
  (cond
   ((null? l) '())
   ((t? (car l)) (cons (car l) (filter t? (cdr l))))
   (else (filter t? (cdr l)))))

(define (interval->predicate i)
  (let(
       (lo (integer->char (car i)))
       (hi (integer->char (cdr i))))
    (if (= (+ 1 (car i)) (cdr i))
        (lambda (c) (char=? c lo))
        (lambda (c) (and (char>=? c lo) (char<? c hi))))))
       
(define (set->predicate set)
  (let(
       (ipred (map interval->predicate set)))
    (lambda (c)
      (let loop ((ipred ipred))
        (cond
         ((null? ipred) #f)
         (((car ipred) c) #t)
         (else (loop (cdr ipred))))))))
          
(define (fsm->table fsm)
  (let(
       (x #f)
       (rename-table #f)
       (table #f))
    
    (set! x -1)
    
    (set! rename-table
          (map (lambda (s)
                 (set! x (+ x 1))
                 (cons s x))
               (cons (fsm-initial-state fsm)
                     (filter (lambda (s) (not (eq? s (fsm-initial-state fsm)))) (fsm-states fsm)))))
    
    (set! table (list->vector
                 (map (lambda (state)
                        (let*(
                              (itable (map (lambda (t) (cons (set->predicate (cadr t)) (cdr (assq (caddr t) rename-table))))
                                           (filter (lambda (t) (eq? (car t) state)) (fsm-transition-table fsm)))))
                          
                          (if (memq state (fsm-final-states fsm))
                              (lambda (str wpos fpos)
                                (if (>= wpos (string-length str)) wpos
                                    (let(
                                         (c (string-ref str wpos)))
                                      (let loop ((itable itable))
                                        (cond
                                         ((null? itable) wpos)
                                         (((caar itable) c) ((vector-ref table (cdar itable)) str (+ wpos 1) wpos))
                                         (else (loop (cdr itable))))))))
                              
                              (lambda (str wpos fpos)
                                (if (>= wpos (string-length str)) fpos
                                    (let(
                                         (c (string-ref str wpos)))
                                      (let loop ((itable itable))
                                        (cond
                                         ((null? itable) fpos)
                                         (((caar itable) c) ((vector-ref table (cdar itable)) str (+ wpos 1) fpos))
                                         (else (loop (cdr itable)))))))))))
                      (map car rename-table))))
    table))

(define (fsm->function fsm)
  (let(
       (table (fsm->table fsm)))
    (lambda (str p0)
      ((vector-ref table 0) str p0 #f))))
  
(define (make-regexp s)
  (fsm->function (run (re) s)))
   
(define (match re s #!optional (p0 0))
  (declare (not safe) (block) (fixnum) (standard-bindings) (extended-bindings))
  (let match ((p0 p0))
    (if (>= p0 (string-length s)) #f
        (let(
             (res (re s p0)))
          (if res (cons p0 res)
              (match (+ p0 1)))))))

(define (find s re #!optional (p0 0))
  (match re s p0))

(define (replace s re s1)
  ;; todo: let s1 be a function with one argument (matched string)
  (declare (not safe) (block) (fixnum) (standard-bindings) (extended-bindings))
  (let(
       (strlen (string-length s)))
    (let replace ((p0 0) (rs '()) (l1 strlen))
      (if (< p0 strlen)
          (let(
               (p1 (re s p0)))
            (if p1
                (replace p1 (cons (cons p0 p1) rs) (+ l1 (string-length s1) (- p0 p1)))
                (replace (+ p0 1) rs l1)))
          (let(
               (result (make-string l1))
               (s1-len (string-length s1)))
            (let build ((p1 l1) (p0 (string-length s)) (rs rs))
              (if (null? rs)
                  (begin
                    (substring-move! s 0 p0 result 0)
                    result)
                  (let*(
                        (x0 (+ p1 (- (cdar rs) p0)))
                        (x1 (- x0 s1-len)))
                    (substring-move! s (cdar rs) p0 result x0)
                    (substring-move! s1 0 s1-len result x1)
                    (build x1 (caar rs) (cdr rs))))))))))

                ;; (let*(
                ;;       (a (caar rs))
                ;;       (b (cdar rs))
                ;;       (x0 (+ p1 (- b p0)))
                ;;       (x1 (- x0 s1-len)))
                ;;   (substring-move! s b p0 result x0)
                ;;   (substring-move! s1 0 s1-len result x1)
                ;;   (build x1 a (cdr rs)))))))))