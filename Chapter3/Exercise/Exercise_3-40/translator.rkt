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
        (let*
          ( [res (apply-senv senv var)]
            [index (car res)]
            [is-rec-var (cdr res)])
        
          (if is-rec-var
            (nameless-letrec-var-exp index)
            (nameless-var-exp index))))
    ;   letrec
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (letrec-exp (p-name b-var p-body body)
      (let
        ( [rec-senv (extend-rec-senv p-name senv)])
        (nameless-letrec-exp
          (nameless-proc-exp
            (translation-of p-body (extend-senv b-var rec-senv)))
          (translation-of body rec-senv))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (const-exp (num) 
        exp)

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
      
      (else report-invalid-source-expression exp)))
)


(define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp))
)