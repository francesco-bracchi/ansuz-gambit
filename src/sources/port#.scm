(##namespace ("ansuz-sources-port#"
              stream-car
              stream-cdr
              stream-pos
              stream-row
              stream-col
              stream))

;; (define-macro (stream-car p)
;;   `(force (car ,p)))

;; (define-macro (stream-cdr p)
;;   `(force (cdr ,p)))

;; (define-macro (stream p)
;;   (let(
;;        (kdr (gensym 'cdr))
;;        (kar (gensym 'car)))
;;     `(let ,kdr ()
;;           (let(
;;                (,kar (delay (read-char ,p))))
;;             (cons
;;              ,kar
;;              (delay (begin (force ,kar) (,kdr))))))))

;; (define-macro (stream-pos st)
;;   `(error "position not supported by port parser"))

;; (define-macro (stream-row st)
;;   `(error "row not supported by port parser"))

;; (define-macro (stream-col st)
;;   `(error "column not supported by port parser"))

(define-macro (stream p)
  `(vector #f #f ,p))

(define-macro (stream-car p)
  (let(
       (ch (gensym 'ch)))
    `(or (vector-ref ,p 0)
	 (let(
	      (,ch (read-char (vector-ref ,p 2))))
	   (vector-set! ,p 0 ,ch)
	   ,ch))))

(define-macro (stream-cdr p)
  (let(
       (kdr (gensym 'kdr)))
    `(or (vector-ref ,p 1)
	 (and (stream-car ,p)
	      (let(
		   (,kdr (vector #f #f (vector-ref ,p 2))))
		(vector-set! ,p 1 ,kdr)
		,kdr)))))

(define-macro (stream-pos st)
  `(error "position not supported by port parser"))

(define-macro (stream-row st)
  `(error "row not supported by port parser"))

(define-macro (stream-col st)
  `(error "column not supported by port parser"))


	       