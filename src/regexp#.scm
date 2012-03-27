(namespace ("ansuz-regexp#"
            regexp
            make-regexp
            match
            find
            replace))

(define-macro (regexp e)
  (include "~~futhark/ansuz/sources/string#.scm")
  (include "~~futhark/ansuz/char-stream-parser#.scm")
  (include "~~futhark/ansuz/re/parser#.scm")
  (include "~~futhark/ansuz/re/fgen#.scm")
  (load "~~futhark/ansuz/re/sets")
  (load "~~futhark/ansuz/re/fsm")
  (load "~~futhark/ansuz/re/parser")
  (load "~~futhark/ansuz/re/fgen")
  (fsm->code (run-monad (re)
                        (stream e)
                        (lambda v (car v))
                        (lambda r (error (car r))))))




(define-macro (init-regexp)

  (##include "~~lib/gambit#.scm")
  (##include "~~lib/_gambit#.scm")

  (input-port-readtable-set!  
   (repl-input-port)
   (let*((readtable0 (input-port-readtable (current-input-port)))
         (readtable (##readtable-copy readtable0)))
     (##readtable-char-class-set!
      readtable
      #\&
      #t
      (lambda (re c)
        (pp x)
        (let ((start-pos (##readenv-current-filepos re)))
          (macro-read-char (macro-readenv-port re)))
        (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
        (macro-readenv-wrap re (list 'regexp))))
     readtable))
  '(begin 42))

(init-regexp)
  