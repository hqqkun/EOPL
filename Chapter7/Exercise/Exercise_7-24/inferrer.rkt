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
      
      (let-exp (vars exps body)
        (let loop ([vars vars] [exps exps] [tenv tenv] [subst subst])
          (if (null? vars)
            (type-of body tenv subst)
            (cases answer (type-of (car exps) tenv subst)
              (an-answer (ty1 subst)
                (loop (cdr vars) (cdr exps)
                  (extend-tenv (car vars) ty1 tenv) subst))))))

      (proc-exp (vars otypes body)
        (let ( [arg-types (map otype->type otypes)])
          (cases answer 
            (type-of body 
              (extend-tenv* vars arg-types tenv) subst)
            (an-answer (result-type subst)
              (an-answer
                (proc-type arg-types result-type) subst)))))

      (call-exp (rator rands)
        (let ([exp-ty (fresh-tvar-type)])
          (cases answer (type-of rator tenv subst)
            (an-answer (ty1 subst)
              (let loop ([rands rands] [subst subst] [lst '()])
                (if (null? rands)
                  (let 
                    ( [new-subst (unifier ty1 (proc-type (reverse lst) exp-ty) subst rator)])
                    (an-answer exp-ty new-subst))
                  (cases answer (type-of (car rands) tenv subst)
                    (an-answer (ty subst)
                      (loop (cdr rands) subst (cons ty lst))))))))))
      
      (letrec-exp (proc-result-otypes proc-names 
                      bvarss proc-arg-otypess 
                      proc-bodys
                      letrec-body)
        (let*
          ( [proc-result-types (map otype->type proc-result-otypes)]
            [proc-arg-typess (map (lambda (vars) (map otype->type vars)) proc-arg-otypess)]
            [tenv-for-letrec-body 
              (extend-tenv* proc-names 
               (map 
                (lambda (args result) (proc-type args result)) 
                proc-arg-typess proc-result-types)
                tenv)])
          (let loop 
            ( [t1s proc-result-types] [t2ss proc-arg-typess] [p-bodys proc-bodys] [subst subst]
              [proc-names proc-names] [bvarss bvarss])
            (if (null? t1s)
              (type-of letrec-body tenv-for-letrec-body subst)
              (let* 
                ( [body (car p-bodys)]
                  [tenv1 (extend-tenv* (car bvarss) (car t2ss) tenv-for-letrec-body)])
                (cases answer (type-of body tenv1 subst)
                  (an-answer (t-res subst)
                    (let ([new-subst (unifier t-res (car t1s) subst body)])
                      (loop (cdr t1s) (cdr t2ss) (cdr p-bodys) new-subst (cdr proc-names)
                        (cdr bvarss))))))))))

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

(define extend-tenv*
  (lambda (vars tys saved-tenv)
    (let loop ( [vars vars] [tys tys] [tenv saved-tenv])
      (if (null? vars)
        tenv
        (loop (cdr vars) (cdr tys)
          (extend-tenv (car vars) (car tys) tenv)))))
)

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
