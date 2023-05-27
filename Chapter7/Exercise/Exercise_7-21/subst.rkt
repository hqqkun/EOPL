#lang eopl

(require "lang.rkt")

(provide init-subst empty-subst extend-subst apply-subst-to-type make-subst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; !substitution is like a store
(define the-subst 'uninit)

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

(define init-subst
  (lambda ()
   (set! the-subst (empty-subst)))
)

(define empty-subst
  (lambda () '())
)

(define extend-subst
  (lambda (tvar ty)
    (set! the-subst (cons (cons tvar ty) the-subst)))
)

(define apply-subst-to-type
  (lambda (ty)
    (cases type ty
      (int-type () ty)
      (bool-type () ty)
      (proc-type (arg-type result-type)
        (proc-type
          (apply-subst-to-type arg-type) 
          (apply-subst-to-type result-type)))
      (tvar-type (sn)
        (let ( [tmp (assoc ty the-subst)])
          (if tmp
            (apply-subst-to-type (cdr tmp))
            ty)))))
)

(define make-subst
  (lambda ()
    (init-subst)
    (extend-subst (tvar-type 2) (tvar-type 1))
    (extend-subst (tvar-type 1) (bool-type))
    (extend-subst 
        (tvar-type 0) (int-type)))
)
