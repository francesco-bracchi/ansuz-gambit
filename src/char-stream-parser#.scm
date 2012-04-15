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
  ))

(include "reflect#.scm")
(include "monad#.scm")
(include "orelse#.scm")
(include "language#.scm")
(include "kernel#.scm")
(include "extras#.scm")

(define-macrop (get-if-char t?)
  ;; todo inline get-if-char
  (let((ch (gensym 'ch)))
    `(get-if (lambda (,ch) (and (char? ,ch) (,t? ,ch))))))

(define-macrop (char c0)
  (let((ch (gensym 'ch)))
    `(get-if-char (lambda (,ch) (char=? ,ch ,c0)))))

(define-macrop (interval lo up)
  (let((ch (gensym 'ch)))
    `(get-if (lambda (,ch) (and (char? ,ch) (char>=? ,ch ,lo) (char<=? ,ch ,up))))))

(define-macrop (upcase)
  `(interval #\A #\Z))

(define-macrop (locase)
  `(interval #\a #\z))

(define-macrop (digit)
  `(get-if-char char-numeric?))

(define-macrop (alpha)
  `(get-if-char char-alphabetic?))

(define-macrop (whitespace)
  `(get-if-char char-whitespace?))

(define-macrop (eos)
  '(get-if eof-object?))

(define-macrop (any)
  (let((e (gensym 'e)))
    `(get-if (lambda (,e) (not (eof-object? ,e))))))
