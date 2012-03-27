
(namespace ("ansuz-re-parser#" regexp define-regexp))


(define-macro (regexp e . x)
  
  (include "~~ansuz/sources/string#.scm")
  (include "~~ansuz/char-stream-parser#.scm")
  (include "~~ansuz/re/parser#.scm")
  (include "~~ansuz/re/cgen#.scm")

  (load "~~ansuz/re/sets")
  (load "~~ansuz/re/fsm")
  (load "~~ansuz/re/parser")
  (load "~~ansuz/re/cgen")
  
  `(with-state ,x ,(fsm->code (run-monad (re)
                                         (stream e)
                                         (lambda v (car v))
                                         (lambda r (error (car r)))))))

(define-macro (define-regexp name val)
  `(define-parser (,name) (regexp ,val)))