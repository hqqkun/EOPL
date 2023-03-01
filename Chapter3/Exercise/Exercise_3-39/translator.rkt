#lang eopl

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide translation-of-program)

; translation-of-program : Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (a-program 
          (translation-of exp1 (init-senv))))))
)

; translation-of : Exp * Senv -> Nameless-exp
(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (var-exp (var) 
        (nameless-var-exp 
          (apply-senv senv var)))
      
      (const-exp (num) 
        (const-exp num))

      (diff-exp (exp1 exp2)
        (diff-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)))

      (zero?-exp (exp1)
        (zero?-exp (translation-of exp1 senv)))
      
      (if-exp (exp1 exp2 exp3)
        (if-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)
          (translation-of exp3 senv)))
      
      (let-exp (var exp1 body)
        (nameless-let-exp
          (translation-of exp1 senv)
          (translation-of 
            body 
            (extend-senv var senv))))

      (proc-exp (var body)
        (nameless-proc-exp
          (translation-of
            body
            (extend-senv var senv))))
      
      (call-exp (rator rand)
        (call-exp 
          (translation-of rator senv)
          (translation-of rand senv)))

      (empty-list-exp () exp)

      (cons-exp (exp1 exp2)
        (cons-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)))
        
      (unpack-exp (vars exp body)
        (value-of-unpack-exp vars exp body senv))

      (else report-invalid-source-expression exp)))
)


(define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp))
)

(define value-of-unpack-exp 
  (lambda (ids exp body senv)
    (letrec
      ( [E  (lambda (_ids)
              (if (null? _ids)
                senv
                (extend-senv (car _ids) (E (cdr _ids))))
            )])
      (nameless-unpack-exp
        (translation-of exp senv)
        ; use new senv to translat body.
        (translation-of body (E ids)))))
)