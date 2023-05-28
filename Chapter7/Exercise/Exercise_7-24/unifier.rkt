#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "data-structures.rkt")
(require "substitutions.rkt")

;; this provides a new view of substitutions, in which unifier
;; replaces extend-env as a constructor.
(provide unifier substitution? empty-subst apply-subst-to-type)

;; we'll maintain the invariant that no variable bound in the
;; substitution occurs in any of the right-hand sides of the
;; substitution. 


;;;;;;;;;;;;;;;; the unifier ;;;;;;;;;;;;;;;;

;; unifier : Type * Type * Subst * Exp -> Subst OR Fails
;; Page: 264
(define unifier
  (lambda (ty1 ty2 subst exp)
    (let ((ty1 (apply-subst-to-type ty1 subst))
          (ty2 (apply-subst-to-type ty2 subst)))
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
            ( [args1 (proc-type->args-type ty1)]
              [args2 (proc-type->args-type ty2)])
            (let loop ([args1 args1] [args2 args2] [subst subst])
              (if (null? args1)
                (unifier
                  (proc-type->result-type ty1)
                  (proc-type->result-type ty2)
                  subst exp)
                (loop (cdr args1) (cdr args2)
                  (unifier (car args1) (car args2) subst exp))))))
        (else (report-unification-failure ty1 ty2 exp))))))

(define report-unification-failure
  (lambda (ty1 ty2 exp) 
    (eopl:error 'unification-failure
      "Type mismatch: ~s doesn't match ~s in ~s~%"
      (type-to-external-form ty1)
      (type-to-external-form ty2)
      exp)))

(define report-no-occurrence-violation
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-no-occurence!
      "Can't unify: type variable ~s occurs in type ~s in expression ~s~%" 
      (type-to-external-form ty1)
      (type-to-external-form ty2)
      exp)))

;; no-occurrence? : Tvar * Type -> Bool
;; usage: Is there an occurrence of tvar in ty?
;; Page: 265
(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
      (int-type () #t)
      (bool-type () #t)
      (proc-type (arg-types result-type)
        (let loop ([arg-types arg-types])
          (if (null? arg-types)
            (no-occurrence? tvar result-type)
            (and
              (no-occurrence? tvar (car arg-types))
              (loop (cdr arg-types))))))
      (tvar-type (serial-number) (not (equal? tvar ty))))))
