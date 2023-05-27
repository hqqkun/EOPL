#lang eopl

(require "lang.rkt")

(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
      (int-type () ty0)
      (bool-type () ty0)
      (proc-type (arg-type result-type)
        (proc-type
          (apply-one-subst arg-type tvar ty1)
          (apply-one-subst result-type tvar ty1)))
      (tvar-type (sn)
        (if (equal? ty0 tvar)
          ty1
          ty0))))
)

(define empty-subst
  (lambda () '())
)

(define extend-subst
  (lambda (subst tvar ty)
    (cons
      (cons tvar ty)
      (map
        (lambda (p)
          (let
            ( [old-lhs (car p)]
              [old-rhs (cdr p)])
            (cons old-lhs (apply-one-subst old-rhs tvar ty))))
        subst)))
)

(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
      (int-type () ty)
      (bool-type () ty)
      (proc-type (arg-type result-type)
        (proc-type
          (apply-subst-to-type arg-type subst)
          (apply-subst-to-type result-type subst)))
      (tvar-type (sn)
        (let ([tmp (assoc ty subst)])
          (if tmp
            (cdr tmp)
            ty)))))
)


(define str "(tvar-type 0 -> tvar-type 1)")
(define str-ty (scan&parse str))
(display (type-to-external-form str-ty))
(newline)

(define subst (extend-subst 
  (extend-subst (empty-subst)
    (tvar-type 0) (int-type))
  (tvar-type 1) (bool-type)))

(display (type-to-external-form (apply-subst-to-type str-ty subst)))