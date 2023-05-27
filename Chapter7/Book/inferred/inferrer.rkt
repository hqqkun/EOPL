#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "data-structures.rkt")
(require "unifier.rkt")

(provide type-of type-of-program)


(define-datatype answer answer?
  (an-answer
    (type type?)
    (subst substitution?))
)

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (cases answer (type-of exp1 (init-tenv) (empty-subst))
          (an-answer (ty subst)
            (apply-subst-to-type ty subst))))
    ))
)



(define type-of
  (lambda (exp tenv subst)
    (cases expression exp
      (const-exp (num)
        (an-answer (int-type) subst))
      
      (var-exp (var) 
        (an-answer (apply-tenv tenv var) subst))

      (zero?-exp (exp1)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (let ([new-subst (unifier ty1 (int-type) subst exp1)])
              (an-answer (bool-type) new-subst)))))

      (diff-exp (exp1 exp2)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst1)
            (let ([subst1 (unifier ty1 (int-type) subst1 exp1)])
              (cases answer (type-of exp2 tenv subst1)
                (an-answer (ty2 subst2)
                  (let ([subst2 (unifier ty2 (int-type) subst2 exp2)])
                    (an-answer (int-type) subst2))))))))
  
      (if-exp (exp1 exp2 exp3)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (let ([subst (unifier ty1 (bool-type) subst exp1)])
              (cases answer (type-of exp2 tenv subst)
                (an-answer (ty2 subst)
                  (cases answer (type-of exp3 tenv subst)
                    (an-answer (ty3 subst)
                      (let ([new-subst (unifier ty2 ty3 subst exp2)])
                        (an-answer ty2 new-subst))))))))))
      
      (let-exp (var exp1 body)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst)
            (type-of body (extend-tenv var ty1 tenv) subst))))

      (proc-exp (var otype body)
        (let ( [arg-type (otype->type otype)])
          (cases answer 
            (type-of body 
              (extend-tenv var arg-type tenv) subst)
            (an-answer (result-type subst)
              (an-answer
                (proc-type arg-type result-type) subst)))))

      (call-exp (rator rand)
        (let ([exp-ty (fresh-tvar-type)])
          (cases answer (type-of rator tenv subst)
            (an-answer (ty1 subst)
              (cases answer (type-of rand tenv subst)
                (an-answer (ty2 subst)
                  (let 
                    ([new-subst 
                      (unifier ty1 (proc-type ty2 exp-ty) subst rand)])
                    (an-answer exp-ty new-subst))))))))
      
      (letrec-exp (proc-result-otype proc-name 
                      bvar proc-arg-otype 
                      proc-body
                      letrec-body)
        (let*
          ( [proc-result-type (otype->type proc-result-otype)]
            [proc-arg-type (otype->type proc-arg-otype)]
            [tenv-for-letrec-body 
              (extend-tenv proc-name 
                (proc-type proc-arg-type proc-result-type)
                tenv)]
            [tenv-for-letproc 
              (extend-tenv bvar proc-arg-type tenv-for-letrec-body)])
          (cases answer (type-of proc-body tenv-for-letproc subst)
            (an-answer (ty1 subst)
              (let 
                ( [subst 
                    (unifier proc-result-type ty1 subst proc-body)])
                (type-of letrec-body tenv-for-letrec-body subst))))))

    ))
)






(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (sym symbol?)
    (type type?)
    (tenv type-environment?)))
  
(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)
  
(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (sym1 val1 old-env)
        (if (eqv? sym sym1) 
          val1
          (apply-tenv old-env sym))))))

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type) 
      (extend-tenv 'v (int-type)
        (extend-tenv 'i (int-type)
          (empty-tenv))))))

;; fresh-tvar-type : () -> Type
;; Page: 265  
(define fresh-tvar-type
  (let ((sn 0))
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))

;; otype->type : OptionalType -> Type
;; Page: 265
(define otype->type
  (lambda (otype)
    (cases optional-type otype
      (no-type () (fresh-tvar-type))
      (a-type (ty) ty))))
