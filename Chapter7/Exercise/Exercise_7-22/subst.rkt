#lang eopl

(require "lang.rkt")
(require racket/base)
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
  (lambda () (make-hash))
)

(define extend-subst
  (lambda (tvar ty)
    (hash-set! the-subst tvar ty))
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
        (if (hash-has-key? the-subst ty)
          (let*
            ( [value (hash-ref the-subst ty)]
              [res (apply-subst-to-type value)])
            (if (equal? value res)
              1
              (hash-set! the-subst ty res))
            res)
          ty))))
)

(define make-subst
  (lambda ()
    (init-subst)
    (extend-subst (tvar-type 2) (tvar-type 1))
    (extend-subst (tvar-type 1) (bool-type))
    (extend-subst 
        (tvar-type 0) (int-type)))
)