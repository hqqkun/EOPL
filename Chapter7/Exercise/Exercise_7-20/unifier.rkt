#lang eopl

(require "lang.rkt")
(require "subst.rkt")

; unifier : Type * Type * Subst * Exp -> Subst
(define unifier
  (lambda (ty1 ty2 subst exp)
    (letrec
      ([A (lambda (ty)
            (cases type ty
              (int-type() ty)
              (bool-type() ty)
              (proc-type (arg-type result-type)
                (proc-type (A arg-type) (A result-type)))
              (tvar-type (sn)
                (apply-subst-to-type ty subst))))])
      (let
        ( [ty1 (A ty1)]
          [ty2 (A ty2)])
      (cond
        ((equal? ty1 ty2) subst)
        ((tvar-type? ty1)
          (if (no-occurrence? ty1 ty2)
            (extend-subst subst ty1 ty2)
            (report-no-occurrence-violation ty1 ty2 exp)))
        ((tvar-type? ty2)
          (if (no-occurrence? ty2 ty1)
            (extend-subst subst ty2 ty1)
            (report-no-occurrence-violation ty2 ty1 exp)))
        ((and (proc-type? ty1) (proc-type? ty2))
          (let
            ([new-subst (unifier
                          (proc-type->arg-type ty1)
                          (proc-type->arg-type ty2) subst exp)])
            (unifier 
              (proc-type->result-type ty1)
              (proc-type->result-type ty2)
              new-subst exp)))
        (else (report-unification-failure ty1 ty2 exp))))
    ))
)

(define report-unification-failure
  (lambda (ty1 ty2 exp) 
    (eopl:error 'unification-failure
      "Type mismatch: ~s doesn't match ~s in ~s~%"
      (type-to-external-form ty1)
      (type-to-external-form ty2)
      exp))
)

(define report-no-occurrence-violation
    (lambda (ty1 ty2 exp)
      (eopl:error 'check-no-occurence!
        "Can't unify: type variable ~s occurs in type ~s in expression ~s~%" 
        (type-to-external-form ty1)
        (type-to-external-form ty2)
        exp))
)

; no tvar occurrence in ty
(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
      (int-type () #t)
      (bool-type () #t)
      (proc-type (arg-type result-type)
        (and
          (no-occurrence? tvar arg-type)
          (no-occurrence? tvar result-type)))
      (tvar-type (sn)
        (not (equal? tvar ty)))))
)
