#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "static-data-structures.rkt")
(require "expand-type.rkt")

(provide type-of)

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp)))
)

;; report-unequal-types : Type * Type * Exp -> Unspecified
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!  
        "Types didn't match: ~s != ~a in~%~a"
        (type-to-external-form ty1)
        (type-to-external-form ty2)
        exp))
)

(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (const-exp (num)
        (int-type))
      (var-exp (var)
        (apply-tenv tenv var))
      
      (qualified-var-exp (m-name var-name) 
        (lookup-qualified-var-in-tenv m-name var-name tenv))

      (diff-exp (exp1 exp2)
        (let
          ( [ty1 (type-of exp1 tenv)]
            [ty2 (type-of exp2 tenv)])
          (check-equal-type! ty1 (int-type) exp1)
          (check-equal-type! ty2 (int-type) exp2)
          (int-type)))
      
       (zero?-exp (exp1)
          (let ((type1 (type-of exp1 tenv)))
            (check-equal-type! type1 (int-type) exp1)
            (bool-type)))
        
        (if-exp (exp1 exp2 exp3)
          (let ((ty1 (type-of exp1 tenv))
                (ty2 (type-of exp2 tenv))
                (ty3 (type-of exp3 tenv)))
            (check-equal-type! ty1 (bool-type) exp1)
            (check-equal-type! ty2 ty3 exp)
            ty2))
        
        (let-exp (var exp1 body)
          (let ([ty1 (type-of exp1 tenv)])
            (type-of body (extend-tenv var ty1 tenv))))
        
        (proc-exp (bvar bvar-type body)
          (let*
            ( [expanded-bvar-type (expand-type bvar-type tenv)]
              [res-type 
                (type-of body 
                  (extend-tenv bvar expanded-bvar-type tenv))])
            (proc-type expanded-bvar-type res-type)))
        
        (call-exp (rator rand)
          (let
            ( [rator-ty (type-of rator tenv)]
              [rand-ty (type-of rand tenv)])
            (cases type rator-ty
              (proc-type (arg-type res-type)
                (check-equal-type! arg-type rand-ty rand)
                res-type)
              (else (eopl:error 'type-of
                  "Rator not a proc type:~%~s~%had rator type ~s"   
                  rator (type-to-external-form rator-ty))))))
    
        (letrec-exp (ty1 p-name b-var ty2 p-body letrec-body)
          (let*
            ( [new-ty1 (expand-type ty1 tenv)]
              [new-ty2 (expand-type ty2 tenv)]
              [new-pty (expand-type (proc-type ty2 ty1) tenv)]
              [tenv-letrec-body 
                (extend-tenv p-name new-pty tenv)]
              [p-body-ty 
                (type-of p-body (extend-tenv b-var new-ty2 tenv-letrec-body))])
            (check-equal-type! p-body-ty new-ty1 p-body)
            (type-of letrec-body tenv-letrec-body)))
    ))
)