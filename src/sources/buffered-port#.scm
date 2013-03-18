(##namespace ("ansuz-sources-buffered-port#"
              stream-car
              stream-cdr
              stream-pos
              stream-row
              stream-col
              stream
	      buffer-size))

(define-macro (buffer-size) 8192)
  
(define-macro (stream p)
  (let((buffer (gensym 'buffer))
       (size (gensym 'size))
       (port (gensym 'port)))
    `(let*((,port ,p)
	   (,buffer (make-string (buffer-size)))
	   (,size (read-substring ,buffer 0 (buffer-size) ,port 1)))
       (vector ,buffer 0 (- ,size 1) #f 0 1 0 ,port))))

(define-macro (stream-car s)
  (let((stream (gensym 'stream))
       (c (gensym 'c)))
    `(let*((,stream ,s)
           (,c (string-ref (vector-ref ,stream 0) (vector-ref ,stream 1))))
       (if (char=? ,c #\nul) #!eof ,c))))

(define-macro (stream-cdr s)
  (let(
       (stream (gensym 'stream))
       (next (gensym 'next)))
  `(let((,stream ,s))
     (declare (fixnum))
     (cond
      ((< (vector-ref ,stream 1) (vector-ref ,stream 2))
       (vector (vector-ref ,stream 0)
	       (+ 1 (vector-ref ,stream 1))
	       (vector-ref ,stream 2)
	       (vector-ref ,stream 3)
	       (vector-ref ,stream 4)
	       (vector-ref ,stream 5)
	       (vector-ref ,stream 6)
	       (vector-ref ,stream 7)))
      ((vector-ref ,stream ,3) => (lambda (s) s))
      (else
       (let((,next (stream (vector-ref ,stream 7))))
	 (vector-set! ,stream 3 ,next)
	 ,next))))))

(define-macro (stream-pos s)
  `(vector-ref ,s 1))

(define-macro (stream-row s) 
  `(error "stream-row not supportet"))

(define-macro (stream-col s)
  `(error "stream-row not supported"))
