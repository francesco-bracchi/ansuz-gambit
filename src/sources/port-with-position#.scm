(##namespace ("ansuz-sources-port-with-position#"
              stream-car
              stream-cdr
              stream-pos
              stream-row
              stream-col
              stream))

(define-macro (stream p)
  `(vector #f #f 0 1 0 ,p))

(define-macro (stream-car p)
  (let(
       (ch (gensym 'ch)))
    `(or (vector-ref ,p ,0)
	 (let((,ch (read-char (vector-ref ,p 5))))
	   (vector-set! ,p 0 ,ch)
	   ,ch))))

(define-macro (stream-cdr p)
  (let(
       (ch (gensym 'ch))
       (kdr (gensym 'kdr)))
    `(or (vector-ref ,p 1)
	 (let*((,ch (stream-car ,p))
	       (,kdr (if (or (eof-object? ,ch) (char=? ,ch #\newline))
			 (vector #f #f (+ (vector-ref ,p 2) 1) (+ (vector-ref ,p 3) 1) 0 (vector-ref ,p 5))
			 (vector #f #f (+ (vector-ref ,p 2) 1) (vector-ref ,p 3) (+ 1 (vector-ref ,p 4)) (vector-ref ,p 5)))))
	   (vector-set! ,p 1 ,kdr)
	   ,kdr))))

(define-macro (stream-pos st)
  `(vector-ref ,st 2))

(define-macro (stream-row st)
  `(vector-ref ,st ,3))

(define-macro (stream-col st)
  `(vector-ref ,st 4))
