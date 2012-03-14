(##namespace ("ansuz-sources-string#"
              stream-car
              stream-cdr
              stream-pos
              stream-row
              stream-col
              stream
              ))

(define-macro (stream-car pr)
  (let(
       (p (gensym 'p))
       (s (gensym 's))
       (x (gensym 'x)))
    `(let* ((,p ,pr)
            (,s (car ,p))
            (,x (cdr ,p)))
       (if (< ,x (string-length ,s))
           (string-ref ,s ,x)
           #!eof))))

(define-macro (stream-cdr pr)
  (let(
       (p (gensym 'p)))
    `(let ((,p ,pr))
       (cons (car ,p) (+ 1 (cdr ,p))))))

(define-macro (stream-pos st)
  `(error "position not supported by string parser"))

(define-macro (stream-row st)
  `(error "row not supported by string parser"))

(define-macro (stream-col st)
  `(error "column not supported by string parser"))

(define-macro (stream s)
  `(cons ,s 0))