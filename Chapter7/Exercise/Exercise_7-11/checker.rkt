#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")

(provide type-of-program)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp)))
)


(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (type-of exp1 (init-tenv)))))
)


(define type-of
  (lambda (exp tenv)
    (cases expression exp
      ;; mutable-pairs
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (begin-exp (exp1 exps)
        (let loop ( [e1 exp1] [es exps])
          (let ([e1-ty (type-of e1 tenv)])
            (if (null? es)
              e1-ty
              (loop (car es) (cdr es))))))
      
      (assign-exp (var exp1)
        (let
          ( [var-ty (apply-tenv tenv var)]
            [exp1-ty (type-of exp1 tenv)])
          (check-equal-type! var-ty exp1-ty exp1)
          (void-type)))
      (newpair-exp (exp1 exp2)
        (let
          ( [ty1 (type-of exp1 tenv)]
            [ty2 (type-of exp2 tenv)])
          (pair-type ty1 ty2)))
      
      (left-exp (exp1)
        (let ([ty1 (type-of exp1 tenv)])
          (cases type ty1
            (pair-type (first _)
              first)
            (else (report-rator-not-a-pair-type ty1 exp1)))))
      
      (right-exp (exp1)
        (let ([ty1 (type-of exp1 tenv)])
          (cases type ty1
            (pair-type (_ second)
              second)
            (else (report-rator-not-a-pair-type ty1 exp1)))))
      
      (setleft-exp (exp1 exp2)
        (let 
          ( [ty1 (type-of exp1 tenv)]
            [ty2 (type-of exp2 tenv)])
          (cases type ty1
            (pair-type (left _)
              (check-equal-type! left ty2 exp1)
              (void-type))
            (else (report-rator-not-a-pair-type ty1 exp1)))))

      (setright-exp (exp1 exp2)
        (let 
          ( [ty1 (type-of exp1 tenv)]
            [ty2 (type-of exp2 tenv)])
          (cases type ty1
            (pair-type (_ right)
              (check-equal-type! right ty2 exp1)
              (void-type))
            (else (report-rator-not-a-pair-type ty1 exp1)))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      

      (const-exp (num)
        (int-type))
      
      (var-exp (var)
        (apply-tenv tenv var))

      (diff-exp (exp1 exp2)
        (let
          ( [ty1 (type-of exp1 tenv)]
            [ty2 (type-of exp2 tenv)])
          (check-equal-type! ty1 (int-type) exp1)
          (check-equal-type! ty2 (int-type) exp2)
          (int-type)))
      
      (zero?-exp (exp1)
        (let
          ( [ty1 (type-of exp1 tenv)])
          (check-equal-type! ty1 (int-type) exp1)
          (bool-type)))

      (if-exp (exp1 exp2 exp3)
        (let
          ( [ty1 (type-of exp1 tenv)]
            [ty2 (type-of exp2 tenv)]
            [ty3 (type-of exp3 tenv)])
          (check-equal-type! ty1 (bool-type) exp1)
          (check-equal-type! ty2 ty3 exp)
          ty2))
      
      (let-exp (var exp1 body)
        (let
          ( [exp1-ty (type-of exp1 tenv)])
          (type-of body (extend-tenv var exp1-ty tenv))))

      (proc-exp (b-var b-var-ty body)
        (let
          ( [body-ty 
              (type-of body (extend-tenv b-var b-var-ty tenv))])
          (proc-type b-var-ty body-ty)))

      (call-exp (rator rand)
        (let
          ( [rator-ty (type-of rator tenv)]
            [rand-ty (type-of rand tenv)])
          (cases type rator-ty
            (proc-type (arg-type result-type)
              (check-equal-type! arg-type rand-ty rand)
              result-type)
            (else (report-rator-not-a-proc-type rator-ty rator)))))
          
      (letrec-exp (ty1 p-name b-var ty2 p-body letrec-body)
        (let*
          ( [tenv-letrec-body 
              (extend-tenv p-name (proc-type ty2 ty1) tenv)]
            [p-body-ty 
              (type-of p-body (extend-tenv b-var ty2 tenv-letrec-body))])
          (check-equal-type! p-body-ty ty1 p-body)
          (type-of letrec-body tenv-letrec-body)))
      ))
)

(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-of-expression
      "Rator not a proc type:~%~s~%had rator type ~s"   
      rator 
      (type-to-external-form rator-type)))
)

(define report-rator-not-a-pair-type
  (lambda (rator-type rator)
    (eopl:error 'type-of-expression
      "Rator not a pair type:~%~s~%had rator type ~s"   
      rator 
      (type-to-external-form rator-type)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utils

(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!
       "Types didn't match: ~s != ~s in~%~a"
       (type-to-external-form ty1)
       (type-to-external-form ty2)
       exp))
)

(define-datatype type-environment type-environment?
   (empty-tenv-record)
    (extended-tenv-record
      (sym symbol?)
      (ty type?)
      (saved-tenv type-environment?))
)

(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (sym1 val1 saved-tenv)
        (if (eqv? sym1 sym)
          val1
          (apply-tenv saved-tenv sym)))
    ))
)



(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type)
      (extend-tenv 'v (int-type)
        (extend-tenv 'i (int-type)
          (empty-tenv)))))
)
