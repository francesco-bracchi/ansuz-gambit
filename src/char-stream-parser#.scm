(##namespace
 ("ansuz-char-stream-parser#"
  get-if-char
  char
  interval
  locase
  upcase
  digit
  alpha
  whitespace
  eos
  any
  word
  separated-values
  ))

(include "reflect#.scm")
(include "monad#.scm")
(include "orelse#.scm")
(include "language#.scm")
(include "kernel#.scm")
(include "extras#.scm")

(define-macro+ (get-if-char t?)
  ;; todo inline get-if-char
  (let(
       (ch (gensym 'ch)))
    `(get-if (lambda (,ch) (and (char? ,ch) (,t? ,ch))))))

(define-macro+ (char c0)
  (let(
       (ch (gensym 'ch)))
    `(get-if-char (lambda (,ch) (char=? ,ch ,c0)))))

(define-macro+ (interval lo up)
  (let(
       (ch (gensym 'ch)))
    `(get-if (lambda (,ch) (and (char? ,ch) (char>=? ,ch ,lo) (char<=? ,ch ,up))))))

(define-macro+ (upcase)
  `(interval #\A #\Z))

(define-macro+ (locase)
  `(interval #\a #\z))

(define-macro+ (digit)
  `(get-if-char char-numeric?))

(define-macro+ (alpha)
  `(get-if-char char-alphabetic?))

(define-macro+ (whitespace)
  `(get-if-char char-whitespace?))

(define-macro+ (eos)
  '(get-if eof-object?))

(define-macro+ (any)
  (let(
       (e (gensym 'e)))
    `(get-if (lambda (,e) (not (eof-object? ,e))))))


(define-macro+ (word w)
  (let(
       (l (gensym 'l))
       (wd (gensym 'wd))
       (j (gensym 'j)))
    `(parser-eval
      (let(
           (,l (string-length ,w)))
        (letrec(
                (,wd (parser (,j)
                             (if (>= ,j ,l) (return ,w)
                                 (>> (char (string-ref ,w ,j))
                                     (,wd (+ ,j 1)))))))
          (,wd 0))))))

(define-macro+ (separated-values sep val)
  (let(
       (sp (gensym 'sp))
       (vl (gensym 'vl))
       (v (gensym 'v))
       (vs (gensym 'vs))
       )
    `(reify (,sp ,sep)
            (reify (,vl ,val)
                   (parser-eval
                    (<> (>> (<- ,v (,vl))
                            (<- ,vs (kleene (>> (,sp) (,vl))))
                            (return (cons ,v ,vs)))
                        (return '())))))))