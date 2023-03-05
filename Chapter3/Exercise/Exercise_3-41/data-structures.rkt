#lang eopl

(require "lang.rkt")

;;;;;;;;;;;;;;;; proc value ;;;;;;;;;;;;;;;;
(define-datatype proc proc?
  (procedure
    (body expression?)
    (saved-env nameless-environment?))
)

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
   (proc proc?))
)

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
      (else (report-expval-extractor-error 'proc val))))
)

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val))))
)

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val))))
)

(define report-expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value))
)


;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;
; MLIR : nameless-environment is used for evaluate nameless program.

; nameless-environment? : SchemeVal -> Bool
(define nameless-environment?
  (lambda (env)
    ((list-of (list-of expval?)) env))
)

; empty-nameless-env : () -> Nameless-env
(define empty-nameless-env
  (lambda ()
    '())
)

; empty-nameless-env? : Nameless-env -> Bool
(define empty-nameless-env? null?)

; extend-nameless-env : ExpVal * Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val env)
   (extend-nameless-env* (list val) env))
)

; extend-nameless-env* : ListOf(ExpVal) * Nameless-env -> Nameless-env
(define extend-nameless-env*
  (lambda (vals env)
    (cons vals env)))

; apply-nameless-env : Nameless-env * Lex-depth * Pos -> ExpVal
(define apply-nameless-env
  (lambda (env lex-depth pos)
    (list-ref (list-ref env lex-depth) pos))
)


;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;

; empty-senv : () -> Senv
(define empty-senv
  (lambda ()
    '())
)

; extend-senv : Var * Senv -> Senv
(define extend-senv
  (lambda (var senv)
   (extend-senv* (list var) senv))
)

; extend-senv* : ListOf(Var) * Senv -> Senv
(define extend-senv*
  (lambda (vars senv)
    (cons vars senv))
)

; apply-senv : Senv * Var -> Pair(Lex-depth, Position)
(define apply-senv
  (lambda (senv search-var)
    (letrec
      ( [A  (lambda (vars index)
              (cond
                ((null? vars) -1)
                ((eqv? (car vars) search-var) index)
                (else (A (cdr vars) (+ index 1)))))])
      
      (if (null? senv)
        (report-unbound-var search-var)
        (let
          ( [first-index (A (car senv) 0)])
          (if (>= first-index 0)
            (cons 0 first-index)
            (let*
              ( [rest-res (apply-senv (cdr senv) search-var)]
                [lex-depth (car rest-res)]
                [index (cdr rest-res)])
              (cons (+ lex-depth 1) index)))))))
)

(define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "unbound variable in code: ~s" var))
)

(provide (all-defined-out)) 