(##namespace ("ansuz-re-sets#"))
(##include "~~/lib/gambit#.scm")

(define (range-diff a b)
  (let(
       (lo-a (car a))
       (up-a (cdr a))
       (lo-b (car b))
       (up-b (cdr b)))
    (cond
     ((or (>= lo-b up-a) (<= up-b lo-a))
      (values (list a) '() (list b)))

     ((and (<= lo-b lo-a) (<= up-b up-a))
      (values (if (= lo-b lo-a) '() (list (cons lo-b lo-a)))
              (if (= lo-a up-b) '() (list (cons lo-a up-b)))
              (if (= up-b up-a) '() (list (cons up-b up-a)))))
     
     ((and (<= lo-b lo-a) (>= up-b up-a))
      (values '()
              (list a)
              (append
               (if (= lo-b lo-a) '() (list (cons lo-b lo-a)))
               (if (= up-a up-b) '() (list (cons up-a up-b))))))
     ((and (>= lo-b lo-a) (<= up-b up-a))
      (values (append
               (if (= lo-a lo-b) '() (list (cons lo-a lo-b)))
               (if (= up-b up-a) '() (list (cons up-b up-a))))
              (list b)
              '()))
     ((and (>= lo-b lo-a) (>= up-b up-a))
      (values (if (= lo-a lo-b) '() (list (cons lo-a lo-b)))
              (if (= lo-b up-a) '() (list (cons lo-b up-a)))
              (if (= up-a up-b) '() (list (cons up-a up-b))))))))

(define (range-set-diff a b)
  (if (null? b)
      (values (list a) '() '())
      (receive (a1 x1 b1) (range-diff a (car b))
        (receive (a2 x2 b2) (set-diff a1 (cdr b))
          (values
           a2
           (append x1 x2)
           (append b1 b2))))))
    
(define (set-diff a b)
  (cond
   ((null? a) (values '() '() b))
   ((null? b) (values a '() '()))
   (else
    (receive (a1 x1 b1) (range-set-diff (car a) b)
      (receive (a2 x2 b2) (set-diff (cdr a) b1)
        (values
         (append a1 a2)
         (append x1 x2)
         b2))))))

(define *max* (expt 2 16))

(define all (list (cons 0 *max*)))

(define (set-complement rs)
  (if (null? rs) all
      (let loop (
                 (k (cdar rs))
                 (rs (cdr rs))
                 (as (if (= 0 (caar rs)) '()  (list (cons 0 (caar rs))))))
        (cond
         ((null? rs)
          (reverse (if (= k *max*) as (cons (cons k *max*) as))))
         (else
          (loop
           (cdar rs)
           (cdr rs)
           (cons (cons k (caar rs)) as)))))))

;; (define (set-union as bs)
;;   (if (null? bs) as
;;       (let(
;;            (b (car bs))
;;            (bs (cdr bs)))
;;         (let coalesce ((lo (car b))
;;                        (hi (cdr b))
;;                        (as as)
;;                        (rs '()))
;;           (if (null? as) `((,lo . ,hi) ,@(reverse rs))
;;               (let*(
;;                     (a (car as))
;;                     (loa (car a))
;;                     (hia (cdr a)))
;;                 (pp `(coal (min ,lo ,loa) (max ,hi ,hia)))
;;                 (if (or (and (>= lo loa) (<= lo hia))
;;                         (and (<= hi hia) (>= hi loa)))
;;                     (coalesce (min lo loa) (max hi hia) (cdr as) rs)
;;                     (coalesce lo hi (cdr as) (cons (car as) rs)))))))))

(define (set-union as bs)
  (if (null? as) bs
      (set-union (cdr as) (coalesce-interval (car as) bs))))

(define (coalesce-interval a bs)
  (cond
   ((null? bs) (list a))
   ((particle-coalesce? a (car bs)) =>
    (lambda (p1) (coalesce-interval p1 (cdr bs))))
   ((<= (car a) (caar bs))
    (cons a bs))
   (else
    (cons (car bs) (coalesce-interval a (cdr bs))))))

(define (particle-coalesce? a b)
  (let(
       (loa (car a))
       (hia (cdr a))
       (lob (car b))
       (hib (cdr b)))
    (and (or (and (>= loa lob) (<= loa hib)) (and (<= hia hib) (>= hia lob)))
         (cons (min loa lob) (max hia hib)))))
   

   

