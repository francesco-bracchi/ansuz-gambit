(##namespace ("ansuz-re-fsm#"))
(##include "~~/lib/gambit#.scm")
(##include "sets#.scm")

;; utility functions
(define (filter t? l)
  (cond
   ((null? l) '())
   ((t? (car l)) (cons (car l) (filter t? (cdr l))))
   (else (filter t? (cdr l)))))

(define (fold f i l)
  (let fold ((i i) (l l))
    (if (null? l) i
        (fold (f i (car l)) (cdr l)))))
 
(define-type fsm
  initial-state
  final-states
  transition-table)

(define (fsm-states f)
  (let loop (
             (ts (fsm-transition-table f))
             (r (list (fsm-initial-state f))))
    (cond
     ((null? ts) (reverse r))
     ((not (memq (caar ts) r))
      (loop ts (cons (caar ts) r)))
     ((not (memq (caddar ts) r))
      (loop ts (cons (caddar ts) r)))
     (else
      (loop (cdr ts) r)))))

(define (fsm-clone fsm)
  (let*(
        (table (map (lambda (s) (cons s (gensym 'c))) (fsm-states fsm)))
        (-> (lambda (s) (cdr (assq s table)))))
    (make-fsm (-> (fsm-initial-state fsm))
              (map -> (fsm-final-states fsm))
              (map (lambda (t) (list (-> (car t)) (cadr t) (-> (caddr t))))
                   (fsm-transition-table fsm)))))

(define (fsm-reachable-states fsm)
  (let reachable ((ss (list (fsm-initial-state fsm)))
                  (vs '()))
    (if (null? ss) (reverse vs)
        (let(
             (s (car ss)))
          (reachable (append (filter (lambda (s) (not (memq s vs))) (map caddr (filter (lambda (t) (eq? (car t) s)) (fsm-transition-table fsm))))
                             (cdr ss))
                     (cons s vs))))))

(define (fsm-clean fsm)
  (let(
       (reachable (fsm-reachable-states fsm)))
    (make-fsm
     (fsm-initial-state fsm)
     (filter (lambda (s) (memq s reachable)) (fsm-final-states fsm))
     (filter (lambda (t) (memq (car t) reachable)) (fsm-transition-table fsm)))))

(define (nfa:// a b)
  (let*(
        (a (fsm-clone a))
        (b (fsm-clone b))
        (s0 (gensym 's0))
        (s1 (gensym 's1)))
     (make-fsm
      s0 (list s1)
      (append
       `((,s0 epsilon ,(fsm-initial-state a))
         (,s0 epsilon ,(fsm-initial-state b)))
       (map (lambda (f) (list f 'epsilon s1)) (fsm-final-states a))
       (map (lambda (f) (list f 'epsilon s1)) (fsm-final-states b))
       (fsm-transition-table a)
       (fsm-transition-table b)))))

;; (define (nfa:++ a b)
;;   (let*(
;;         (x (gensym 'x))
;;         (a (fsm-replace (fsm-final-state a) x a))
;;         (b (fsm-replace (fsm-initial-state b) x b)))
;;     (make-fsm
;;      (fsm-initial-state s0)
;;      (fsm-final-state s1)
;;      (append (fsm-transition-table a)
;;              (fsm-transition-table b)))))

(define (nfa:++ a b)
  (let(
       (a (fsm-clone a))
       (b (fsm-clone b)))
    (make-fsm
     (fsm-initial-state a) (fsm-final-states b)
     (append
      (map (lambda (f) (list f 'epsilon (fsm-initial-state b)))
           (fsm-final-states a))
      (fsm-transition-table a)
      (fsm-transition-table b)))))

(define (nfa:kleene a)
  (let(
       (a (fsm-clone a))
       (s0 (gensym 's0)))
    (make-fsm
      s0 (list s0)
      (append
       `((,s0 epsilon ,(fsm-initial-state a)))
       (map (lambda (f) (list f 'epsilon s0)) (fsm-final-states a))
       (fsm-transition-table a)))))

(define (nfa:repeat p lo up)
  (cond
   ((> lo 0)
    (nfa:++ p (nfa:repeat p (- lo 1) (if (eq? up 'inf) up (- up 1)))))
   ((eq? up 'inf)
    (nfa:kleene p))
   ((> up 0)
    (nfa:// (nfa:++ p (nfa:repeat p 0 (- up 1)))
            (nfa:empty)))
   (else
    (nfa:empty))))

;; (define (nfa:1+kleene a)
;;   (let(
;;        (s0 (gensym 's0))
;;        (s1 (gensym 's1)))
;;     (make-fsm
;;      s0 (list s1)
;;      (append
;;       `((,s0 epsilon ,(fsm-initial-state a))
;;         (,s1 epsilon ,s0))
;;       (map (lambda (f) (list f 'epsilon s1)) (fsm-final-states a))
;;       (fsm-transition-table a)))))

;; (define nfa:empty
;;   (let(
;;        (s0 (gensym 's0))
;;        (s1 (gensym 's1)))
;;     (make-fsm
;;      s0 (list s1)
;;      `((,s0 epsilon ,s1)))))

(define (nfa:empty)
  (let(
       (s0 (gensym 's0)))
    (make-fsm s0 (list s0) '())))

(define (nfa:set set)
  (let(
       (s0 (gensym 's0))
       (s1 (gensym 's1)))
    (make-fsm
     s0 (list s1)
     `((,s0 ,set ,s1)))))


;; (define (step1 a)
;;   (make-fsm
;;    (fsm-initial-state a)
;;    (fsm-final-states a)
;;    (apply
;;     append
;;     (cons
;;      (fsm-transition-table a)
;;      (map
;;       (lambda (t0)
;;         (map
;;          (lambda (t2)
;;            (list (car t0) (cadr t2) (caddr t2)))
;;          (filter (lambda (t1) (eq? (car t1) (caddr t0)))
;;                  (fsm-transition-table a))))
;;       (filter
;;        (lambda (t) (eq? (cadr t) 'epsilon))
;;        (fsm-transition-table a)))))))

;; (define (fna- a)
;;   (make-fsm
;;    (fsm-initial-state a)
;;    (append
;;     (map car
;;          (filter (lambda (t)
;;                    (and (eq? (cadr t) 'epsilon)
;;                         (memq (caddr t) (fsm-final-states a))))
;;                  (fsm-transition-table a)))
;;     (fsm-final-states a))
;;     (fsm-transition-table a)))

;; (define (all t? l)
;;   (cond
;;    ((null? l) #t)
;;    ((t? (car l)) #f)
;;    (else (all ?t (cdr l)))))
    
;; (define (rule1 fsm t1 t2)
;;   (pp `(rule1 ,t1 ,t2))
;;   (and
;;    (eq? (cadr t1) 'epsilon)
;;    (eq? (caddr t1) (car t2))
;;    (all (lambda (t)
;;           (not
;;            (and (eq? (car t) (car t1))
;;                 (eq? (car t) (cadr t1))
;;                 (eq? (caddr t) (caddr t1)))))
;;         (fsm-transition-table fsm))))

;; (define (rule2 fsm t)
;;   (and
;;    (eq? (cadr t) 'epsilon)
;;    (memq (caddr t) (fsm-final-states fsm))
;;    (not (memq (car t) (fsm-final-states fsm)))))


;; (define (apply-rule1 a)
;;   (let loop (
;;              (ts (fsm-transition-table a))
;;              (qs (fsm-transition-table a)))
;;     (cond
;;      ((null? ts) a)
;;      ((null? qs)
;;       (loop (cdr ts)
;;             (fsm-transition-table a)))
;;      ((eq? (car ts) (car qs))
;;       (loop ts (cdr qs)))
;;      ((rule1 a (car ts) (car qs))
;;       (pp 'x)
;;       (fna->nfa-w/o-e
;;        (make-fsm
;;         (fsm-initial-state a)
;;         (fsm-final-states a)
;;         `((,(caar ts) (cadar qs) (caddar qs))
;;           ,@(fsm-transition-table a)))))
;;      (else
;;       (loop ts (cdr qs))))))

;; (define (apply-rule2 a)
;;   (let loop ((ts (fsm-transition-table a)))
;;     (cond
;;      ((null? ts) a)
;;      ((rule2 a (car ts))
;;       (apply-rule2
;;        (make-fsm
;;         (fsm-initial-state a)
;;         (cons (car ts) (fsm-final-states a))
;;         (fsm-transition-table a))))
;;      (else
;;       (loop (cdr ts))))))


;; (define (nfa->nfa-w/o-e a)
;;   (let loop (
;;              (edges
;;               (filter
;;                (lambda (t) (eq? (cadr t) 'epsilon))
;;                (fsm-transition-table a)))
;;              (is (fsm-initial-state a))
;;              (fs (fsm-final-states a))
;;              (ts (filter
;;                   (lambda (t) (not (eq? (cadr t) 'epsilon)))
;;                   (fsm-transition-table a))))
;;     (if (null? edges)
;;         (make-fsm is fs (filter (lambda (t) (not (eq? (cadr t) 'epsilon))) ts))
;;         (let*(
;;               (edge (car edges))
;;               (ts1 (map (lambda (t) (list (car edge) (cadr t) (caddr t)))
;;                         (filter (lambda (t) (eq? (car t) (caddr edge)))
;;                                 ts)))
;;               (ts2 (filter (lambda (t) (not (eq? (car t) (caddr edge))))
;;                            ts)))
;;           (loop (cdr edges)
;;                 is
;;                 (if (and (memq (caddr edge) fs) (not (memq (car edge) fs)))
;;                     (cons (car edge) fs)
;;                     fs)
;;                 (append ts1 ts2))))))

;; (define (nfa->nfa-w/o-e a)
;;   (let loop (
;;              (is (fsm-initial-state a))
;;              (fs (fsm-final-states a))
;;              (ts (fsm-transition-table a)))
;;     (if (not (memq 'epsilon (map cadr ts)))
;;         (make-fsm is fs ts)
;;         (let*(
;;               (t (car (filter (lambda (t) (eq? (cadr t) 'epsilon)) ts)))
;;               (ts (filter (lambda (t0) (not (eq? t t0))) ts)))
;;           (loop is
;;                 (if (and (memq (caddr t) fs) (not (memq (car t) fs)))
;;                     (cons (car t) fs)
;;                     fs)
;;                 (map (lambda (t0)
;;                        (if (eq? (car t0) (caddr t))
;;                            (cons (car t) (cdr t0))
;;                            t0))
;;                      ts))))))

(define (nfa->nfa-w/o-e a)
  (let loop (
             (fs (fsm-final-states a))
             (ts (fsm-transition-table a)))
    (if (not (memq 'epsilon (map cadr ts)))
        (make-fsm (fsm-initial-state a) fs ts)
        (let*(
              (t (car (filter (lambda (t) (eq? (cadr t) 'epsilon)) ts)))
              (ts (filter (lambda (t0) (not (eq? t t0))) ts)))
          (loop (if (and (memq (caddr t) fs) (not (memq (car t) fs)))
                    (cons (car t) fs)
                    fs)
                (append
                 (map (lambda (t0) (cons (car t) (cdr t0)))
                      (filter (lambda (t0) (eq? (car t0) (caddr t))) ts))
                 ts))))))

(define (union a b)
  (cond
   ((null? a) b)
   ((memq (car a) b)
    (union (cdr a) b))
   (else
    (union (cdr a) (cons (car a) b)))))

(define (all fn? l)
  (let all ((l l))
    (cond
     ((null? l) #t)
     ((fn? (car l)) (all (cdr l)))
     (else #f))))

(define (same-set? a b)
  (or
   (and (null? a) (null? b))
   (and (pair? a) (pair? b)
        (all (lambda (x) (memq x b)) a)
        (all (lambda (x) (memq x a)) b))))
    
;; (define (resolve-conflicts ts)
;;   (let loop ((qs ts) (rs ts))
;;     (pp `(resolve ,qs ,rs))
;;     (cond
;;      ((null? qs)
;;       ts)
;;      ((null? rs)
;;       (loop (cdr qs) ts))

;;      ((eq? (car qs) (car rs))
;;       (loop qs (cdr rs)))
     
;;      ((same-set? (caar qs) (caar rs))
;;       (receive (a x b) (set-diff (cadar qs) (cadar rs))
;;         (if (null? x)
;;             (loop qs (cdr rs))
;;             (let(
;;                  (rest (filter (lambda (t)
;;                                  (not
;;                                   (or
;;                                    (eq? t (car qs))
;;                                    (eq? t (car rs)))))
;;                                ts))
;;                  (s1 (union (caddar qs) (caddar rs))))
;;               (resolve-conflicts
;; 		(append
;; 		 `((,(caar qs) ,x ,s1))
;; 		 (if (pair? a) `((,(caar qs) ,a ,(caddar qs))) '())
;; 		 (if (pair? b) `((,(caar rs) ,b ,(caddar rs))) '())
;; 		 (map (lambda (t)
;; 			(list s1 (cadr t) (caddr t)))
;; 		      (filter
;; 		       (lambda (t)
;; 			 (or (same-set? (car t) (caddar qs))
;;                             (same-set? (car t) (caddar rs))))
;; 		       rest))
;; 		 (filter (lambda (t)
;; 			   (not (or
;; 				 (and (null? a) (same-set? (car t) (caddar qs)))
;; 				 (and (null? b) (same-set? (car t) (caddar rs))))))
;; 			 rest)))))))
;;      (else
;;       (loop qs (cdr rs))))))

(define (find-conflict-with t0 ts)
  (let find-conflict-with ((ts ts))
    (cond
     ((null? ts) #f)
     ((same-set? (car t0) (caar ts))
      (receive (a x b) (set-diff (cadr t0) (cadar ts))
	       (if (not (null? x))
		   (car ts)
		   (find-conflict-with (cdr ts)))))
     (else (find-conflict-with (cdr ts))))))

(define (find-conflict ts)
  (if (null? ts) (values #f #f)
      (let*(
	    (t0 (car ts))
	    (t1 (find-conflict-with t0 (cdr ts))))
	(if t1 (values t0 t1)
	    (find-conflict (cdr ts))))))
	     
(define (resolve-conflicts ts)
  (receive (t0 t1) (find-conflict ts)
	   (if (and t0 t1)
	       (resolve-conflicts 
		(resolve-conflict 
		 t0 t1
		 (filter (lambda (t) (not (or (eq? t t0) (eq? t t1)))) ts)))
	       ts)))

(define (resolve-conflict t0 t1 ts) 
  (cond
   ((same-set? (caddr t0) (caddr t1))
    (cons (list (car t0) (set-union (cadr t0) (cadr t1)) (caddr t0)) ts))
   (else
    (receive (a x b) (set-diff (cadr t0) (cadr t1))
             (let((union-state (union (caddr t0) (caddr t1))))
; XXX: double check this if
               (if (or (same-set? union-state (caddr t0)) (same-set? union-state (caddr t1)))
                   (append (list (list (car t0) x union-state)) ts)
                   (let((table-1 
                         (map (lambda (t) (list union-state (cadr t) (caddr t)))
                              (append (filter (lambda (t) (same-set? (car t) (caddr t0))) ts)
                                      (filter (lambda (t) (same-set? (car t) (caddr t1))) ts)))))
                     (append
                      (list (list (car t0) x union-state))
                      (if (null? a) '() (list (list (car t0) a (caddr t0))))
                      (if (null? b) '() (list (list (car t1) b (caddr t1))))
                      table-1
                      ts))))))))

(define (with-sets-fna->dfa fsm)
  (make-fsm
   (fsm-initial-state fsm)
   (fsm-final-states fsm)
   (resolve-conflicts (fsm-transition-table fsm))))

(define (sets->states fsm)
  (let*(
        (states (fsm-states fsm))
        (new-states (map (lambda (_) (gensym 'r)) states))
        (tbl (map cons states new-states))
        (ren (lambda (s)
               (let loop ((tbl tbl))
                 (cond
                  ((null? tbl) #f)
                  ((same-set? (caar tbl) s) (cdar tbl))
                  (else
                   (loop (cdr tbl)))))))
        (initial-state (ren (fsm-initial-state fsm)))
        (final-states
         (map ren
              (filter
               (lambda (s)
                 (let loop ((s s) (f (map car (fsm-final-states fsm))))
                   (cond
                    ((null? s) #f)
                    ((null? f)
                     (loop (cdr s) (map car (fsm-final-states fsm))))
                    ((eq? (car s) (car f))
                     #t)
                    (else (loop s (cdr f))))))
               states)))
        ;; (__ (pp states))
        ;; (__ (pp (fsm-final-states fsm)))
        (transitions
         (filter
          (lambda (t)
            (and (memq (car t) new-states)
                 (pair? (cadr t))))
          (map (lambda (t)
                 (list (ren (car t)) (cadr t) (ren (caddr t))))
               (fsm-transition-table fsm)))))
    (make-fsm
     initial-state
     final-states
     transitions)))

(define (states->sets fsm)
  (make-fsm
   (list (fsm-initial-state fsm))
   (map list (fsm-final-states fsm))
   (map (lambda (t)
          (list (list (car t))
                (cadr t)
                (list (caddr t))))
        (fsm-transition-table fsm))))

(define (nfa-w/o-e->dfa fsm)
  (sets->states
   (with-sets-fna->dfa
    (states->sets fsm))))

;; (define (fsm-simplify-arcs fsm)
;;   (let simplify ((ts (fsm-transition-table fsm))
;;                  (rs '()))
;;     (if (null? ts)
;;         (make-fsm (fsm-initial-state fsm) (fsm-final-states fsm) (reverse rs))
;;         (let*(
;;               (t (car ts))
;;               (ts (cdr ts))
;;               (us (filter (lambda (u) (and (eq? (car u) (car t)) (eq? (caddr u) (caddr t)))) ts)))
;;           (if (null? us)
;;               (simplify (cdr ts) (cons t rs))
;;               (simplify (filter (lambda (u) (not (and (eq? (car u) (car t))) (eq? (caddr u) (caddr t)))) ts)
;;                         (cons (list (car t) (fold set-union '() (map cadr us)) (caddr t)) rs)))))))
                        
       
;; (define (fsm-clean fsm)
;;   (let(
;;        (fs0 (fsm-final-states fsm))
;;        (orig (fsm-transition-table fsm)))
;;     (let clean ((ss (list (fsm-initial-state fsm)))
;;                 (vs '())
;;                 (new '()))
;;       (if (null? ss)
;;           (make-fsm (fsm-initial-state fsm) (fsm-final-states fsm) new)
;;           (let*(
;;                 (s (car ss))
;;                 (ts (filter (lambda (t) (eq? (car t) s)) orig)))
;;             (clean
;;              (append (cdr ss) (filter (lambda (s) (not (memq s vs))) (map caddr ts)))
;;              (cons s vs)
;;              (append ts new)))))))
    
;; (define (nfa-w/o-e->dfa fsm)
;;   (let resolve ((fs (fsm-final-states fsm))
;;                 (ts (fsm-transition-table fsm))
;;                 (rs '()))
;;     (if (null? ts)
;;         (begin
;;           (make-fsm (fsm-initial-state fsm) fs (reverse rs)))
;;         (let(
;;              (t (car ts))
;;              (ts (cdr ts)))
;;           (let find-conflict ((us ts) (vs '()))
;;             (if (null? us) (resolve fs ts (cons t rs))
;;                 (let(
;;                      (u (car us))
;;                      (us (cdr us)))
;;                   (if (not (eq? (car t) (car u)))
;;                       (find-conflict us (cons u vs))
;;                       (receive (tx x ux) (set-diff (cadr t) (cadr u))
;;                         (if (null? x)
;;                             (find-conflict us (cons u vs))
;;                             (let*(
;;                                   (xs (gensym 'xs)))
;;                               (resolve
;;                                (if (or (memq (caddr t) fs) (memq (caddr u) fs)) (cons xs fs) fs)
;;                                (append
;;                                 (list (list (car t) x xs))
;;                                 (if (pair? tx) (list (list (car t) tx (caddr t))) '())
;;                                 (if (pair? ux) (list (list (car u) ux (caddr u))) '())
;;                                 (map (lambda (a) (cons xs (cdr a)))
;;                                      (filter (lambda (a) (or (eq? (car a) (caddr t)) (eq? (car a) (caddr u))))
;;                                              (append (list t u) us vs rs)))
;;                                 us vs rs)
;;                                '()))))))))))))

(define (nfa->dfa fsm)
  (let*((state0 (fsm-clean (nfa->nfa-w/o-e fsm)))
        (state1 (nfa-w/o-e->dfa state0)))
    (fsm-clean state1)))

