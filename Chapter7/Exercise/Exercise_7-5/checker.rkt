#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")

(provide type-of-program)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (type-of exp1 (init-tenv)))))
)


(define type-of
  (lambda (exp tenv)
    (cases expression exp
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
      
      (let-exp (vars exps body)
        (let
          ( [tys (map (lambda (exp) (type-of exp tenv)) exps)])
          (type-of body (extend-tenv* vars tys tenv))))

      (proc-exp (b-vars b-var-tys body)
        (let
          ( [body-ty 
              (type-of body (extend-tenv* b-vars b-var-tys tenv))])
          (proc-type b-var-tys body-ty)))

      (call-exp (rator rands)
        (let
          ( [rator-ty (type-of rator tenv)]
            [rands-tys (map (lambda (exp) (type-of exp tenv)) rands)])
          (cases type rator-ty
            (proc-type (arg-types result-type)
              (check-equal-types! arg-types rands-tys rands)
              result-type)
            (else (report-rator-not-a-proc-type rator-ty rator)))))
          
      (letrec-exp (ty1s p-names b-varss ty2ss p-bodys letrec-body)
        (check-proc-args! ty1s p-names b-varss ty2ss p-bodys tenv)
        (let*
          ( [proc-types (map proc-type ty2ss ty1s)]
            [new-tenv (extend-tenv* p-names proc-types tenv)])
          (type-of letrec-body new-tenv)))))
)


(define check-proc-args!
  (lambda (ty1s p-names b-varss ty2ss p-bodys saved-tenv)
    (let loop 
      ( [ty1s ty1s] [p-names p-names] [b-varss b-varss]
        [ty2ss ty2ss] [p-bodys p-bodys])
      (if (null? ty1s)
        #t
        (begin
          (let 
            ([new-tenv 
              (extend-tenv* 
                (cons (car p-names) (car b-varss)) 
                (cons (proc-type (car ty2ss) (car ty1s)) (car ty2ss)) saved-tenv)]) 
            (check-equal-type! (car ty1s) (type-of (car p-bodys) new-tenv) (car p-bodys)))
          (loop (cdr ty1s) (cdr p-names) (cdr b-varss) (cdr ty2ss) (cdr p-bodys))
        ))))
)

(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-of-expression
      "Rator not a proc type:~%~s~%had rator type ~s"   
      rator 
      (type-to-external-form rator-type)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utils

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type ()  'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
        (list
          (type-to-external-form arg-type)
          '->
          (type-to-external-form result-type)))
    ))
)

(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!
       "Types didn't match: ~s != ~s in~%~a"
       (type-to-external-form ty1)
       (type-to-external-form ty2)
       exp))
)

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp)))
)

(define check-equal-types!
  (lambda (ty1s ty2s exps)
    (let loop ( [ty1s ty1s] [ty2s ty2s] [exps exps])
      (if (null? ty1s)
        #t
      (begin
        (check-equal-type! (car ty1s) (car ty2s) (car exps))
        (loop (cdr ty1s) (cdr ty2s) (cdr exps))))))
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


(define extend-tenv*
  (lambda (vars tys saved-tenv)
    (let loop ( [vars vars] [tys tys] [tenv saved-tenv])
      (if (null? vars)
        tenv
        (loop (cdr vars) (cdr tys)
          (extend-tenv (car vars) (car tys) tenv)))))
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
