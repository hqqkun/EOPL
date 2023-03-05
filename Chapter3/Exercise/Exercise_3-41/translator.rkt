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
          ( [p (apply-senv senv var)]
            [lex-depth (car p)]
            [index (cdr p)])
          (nameless-var-exp lex-depth index)))
      
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
      
      (let-exp (var-lst exp-lst body)
        (nameless-let-exp
          (map (lambda (exp) (translation-of exp senv)) exp-lst)
          (translation-of 
            body 
            (extend-senv* var-lst senv))))

      (proc-exp (var-lst body)
        (nameless-proc-exp
          (translation-of
            body
            (extend-senv* var-lst senv))))
      
      (call-exp (rator rands)
        (call-exp 
          (translation-of rator senv)
          (map (lambda (rand) (translation-of rand senv)) rands)))
      
      (else report-invalid-source-expression exp)))
)


(define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp))
)