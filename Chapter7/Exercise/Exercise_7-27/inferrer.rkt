#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "data-structures.rkt")
(require "unifier.rkt")

(provide type-of type-of-program)


(define-datatype answer answer?
  (an-answer
    (type type?)
    (equations (list-of equation?)))
)

(define small-list-of
  (lambda (pred1 pred2 pred3)
    (lambda (val)
      (and 
        (pair? val) 
        (pred1 (car val)) 
        (pred2 (cadr val))
        (pred3 (caddr val)))))
)

(define equation? (small-list-of type? type? expression?))

; Exercise 7-27
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;! generate a set of equations
(define extend-eq
  (lambda (left right exp old-eqs)
    (cons (list left right exp) old-eqs))
)

(define empty-eq
  (lambda () '())
)


(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (find-final-type 
          (type-of exp1 (init-tenv) (empty-eq))))))
)

(define find-final-type
  (lambda (ans)
    (cases answer ans
      (an-answer (ty equations)
        (let loop ( [eqs equations] [subst (empty-subst)]) 
          (if (null? eqs)
            (apply-subst-to-type ty subst)
            (let*
              ( [first-eq (car eqs)]
                [left (car first-eq)]
                [right (cadr first-eq)]
                [exp (caddr first-eq)]
                [subst (unifier left right subst exp)])
              (loop (cdr eqs) subst)))))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define type-of
  (lambda (exp tenv equations)
    (cases expression exp
      (const-exp (num)
        (an-answer (int-type) equations))
      
      (var-exp (var) 
        (an-answer (apply-tenv tenv var) equations))

      (zero?-exp (exp1)
        (cases answer (type-of exp1 tenv equations)
          (an-answer (ty1 equations)
            (let ([equations (extend-eq ty1 (int-type) exp1 equations)])
              (an-answer (bool-type) equations)))))

      (diff-exp (exp1 exp2)
        (cases answer (type-of exp1 tenv equations)
          (an-answer (ty1 equations)
            (let ( [equations (extend-eq ty1 (int-type) exp1 equations)])
              (cases answer (type-of exp2 tenv equations)
                (an-answer (ty2 equations)
                  (let ( [equations (extend-eq ty2 (int-type) exp2 equations)])
                    (an-answer (int-type) equations))))))))
  
      (if-exp (exp1 exp2 exp3)
        (cases answer (type-of exp1 tenv equations)
          (an-answer (ty1 equations)
            (let ([equations (extend-eq ty1 (bool-type) exp1 equations)])
              (cases answer (type-of exp2 tenv equations)
                (an-answer (ty2 equations)
                  (cases answer (type-of exp3 tenv equations)
                    (an-answer (ty3 equations)
                      (let ([equations (extend-eq ty2 ty3 exp2 equations)])
                        (an-answer ty2 equations))))))))))
      
      (let-exp (var exp1 body)
        (cases answer (type-of exp1 tenv equations)
          (an-answer (ty1 equations)
            (type-of body (extend-tenv var ty1 tenv) equations))))

      (proc-exp (var otype body)
        (let ( [arg-type (otype->type otype)])
          (cases answer 
            (type-of body 
              (extend-tenv var arg-type tenv) equations)
            (an-answer (result-type equations)
              (an-answer
                (proc-type arg-type result-type) equations)))))

      (call-exp (rator rand)
        (let ([exp-ty (fresh-tvar-type)])
          (cases answer (type-of rator tenv equations)
            (an-answer (ty1 equations)
              (cases answer (type-of rand tenv equations)
                (an-answer (ty2 equations)
                  (let 
                    ([equations 
                       (extend-eq ty1 (proc-type ty2 exp-ty) rand equations)])
                    (an-answer exp-ty equations))))))))
      
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
          (cases answer (type-of proc-body tenv-for-letproc equations)
            (an-answer (ty1 equations)
              (let 
                ( [equations
                    (extend-eq ty1 proc-result-type proc-body equations)])
                (type-of letrec-body tenv-for-letrec-body equations))))))
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
