(##namespace ("ansuz-expressions#"))
(##include "~~/lib/gambit#.scm")

(include "reflect#.scm")
(include "monad#.scm")
(include "orelse#.scm")
(include "kernel#.scm")
(include "language#.scm")

(declare
 (standard-bindings)
 (extended-bindings)
 (block)
 ;; (not safe)
 )

(define-structure operator-table
  (prefix read-only: unprintable:)
  (infix read-only: unprintable:)
  (postfix read-only: unprintable:))

(define-structure result macros: precedence function associativity)

(define-parser (prefix ot)
  (prefix* (operator-table-prefix ot)))

(define-parser (prefix* ts)
  (if (null? ts)
      (fail "prefix failed")
      (let*((pars (caar ts))
            (prec (cadar ts)))
        (alt (cat (<- func (pars))
		  (ret (make-result prec func 'none)))
	     (prefix* (cdr ts))))))

(define-parser (infix ot)
  (infix* (operator-table-infix ot)))

(define-parser (infix* ts)
  (if (null? ts) (fail "infix failed")
      (let((pars (caar ts))
           (prec (cadar ts))
           (assc (caddar ts)))
        (alt (cat (<- func (pars))
		  (ret (make-result prec func assc)))
	     (infix* (cdr ts))))))

(define-parser (postfix ot)
  (postfix* (operator-table-postfix ot)))

(define-parser (postfix* ts)
  (if (null? ts) (fail "postfix failed")
      (let((pars (caar ts))
           (prec (cadar ts)))
        (alt (cat (<- func (pars))
		  (ret (make-result prec func 'none)))
	     (postfix* (cdr ts))))))

(define-parser (expr* p ot tp)
  (<- t (term ot tp))
  (expr+ p t ot tp))

(define-parser (expr+ p t0 ot tp)
  (alt (postfix-expr+ p t0 ot tp)
       (infix-expr+ p t0 ot tp)
       (ret t0)))

(define (left? op) (eq? (result-associativity op) 'left))

(define-parser (infix-expr+ p t0 ot tp)
  (<- op (infix ot))
  (if (< (result-precedence op) p) (fail "infix-expr+")
      (cat (<- t1 (expr* (if (left? op)
			     (+ 1 (result-precedence op))
			     (result-precedence op))
			 ot tp))
	   (expr+ p ((result-function op) t0 t1) ot tp))))

(define-parser (postfix-expr+ p t0 ot tp) ;; :-)X
  (<- op (postfix ot))
  (if (< (result-precedence op) p)
      (fail "postfix-expr+")
      (expr+ p ((result-function op) t0) ot tp)))

(define-parser (term ot tp) 
  (alt (prefix-expr ot tp)
       (tp)))

(define-parser (prefix-expr ot tp)
  (<- op (prefix ot))
  (<- e (expr* (result-precedence op) ot tp))
  (ret ((result-function op) e)))

(define-parser (expr ot tp) (expr* 0 ot tp))

(define (operator-table-add-prefix ot d)
  (make-operator-table
   (cons d (operator-table-prefix ot))
   (operator-table-infix ot)
   (operator-table-postfix ot)))

(define (operator-table-add-infix ot d)
  (make-operator-table
   (operator-table-prefix ot)
   (cons d (operator-table-infix ot))
   (operator-table-postfix ot)))

(define (operator-table-add-postfix ot d)
  (make-operator-table
   (operator-table-prefix ot)
   (operator-table-infix ot)
   (cons d (operator-table-postfix ot))))

(define operator-table-empty (make-operator-table '() '() '()))