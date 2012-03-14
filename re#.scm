
(namespace ("ansuz-re-parser#" regexp define-regexp))


(define-macro (regexp e . x)
  
  (include "~~futhark/ansuz/sources/string#.scm")
  (include "~~futhark/ansuz/char-stream-parser#.scm")
  (include "~~futhark/ansuz/re/parser#.scm")
  (include "~~futhark/ansuz/re/cgen#.scm")

  (load "~~futhark/ansuz/re/sets")
  (load "~~futhark/ansuz/re/fsm")
  (load "~~futhark/ansuz/re/parser")
  (load "~~futhark/ansuz/re/cgen")
  
  `(with-state ,x ,(fsm->code (run-monad (re)
                                         (stream e)
                                         (lambda v (car v))
                                         (lambda r (error (car r)))))))

(define-macro (define-regexp name val)
  `(define-parser (,name) (regexp ,val)))