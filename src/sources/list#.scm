(##namespace ("ansuz-sources-list#"
              stream-car
              stream-cdr
              stream-pos
              stream-row
              stream-col
              stream))

(define-macro (stream-car st)
  (let(
       (s (gensym 's)))
    `(let ((,s ,st))
       (if (null? ,s) #!eof (car ,s)))))
    
(define-macro (stream-cdr st)
  (let(
       (s (gensym 's)))
    `(let ((,s ,st)) (if (null? ,s) '() (cdr ,s)))))

(define-macro (stream-pos st)
  `(error "position not supported by list parser"))

(define-macro (stream-row st)
  `(error "row not supported by list parser"))

(define-macro (stream-col st)
  `(error "column not supported by list parser"))

(define-macro (stream s) s)