#lang eopl

(require "lang.rkt")
(provide (all-defined-out))


(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv 
    (bvar symbol?)
    (bval type?)
    (saved-tenv type-environment?))
  (extend-tenv-with-module
    (name symbol?)
    (interface interface?)
    (saved-tenv type-environment?))
)

(define extend-tenv*
  (lambda (vars tys saved-tenv)
    (let loop ( [vars vars] [tys tys] [tenv saved-tenv])
      (if (null? vars)
        tenv
        (loop (cdr vars) (cdr tys)
          (extend-tenv (car vars) (car tys) tenv)))))
)


(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let
      ( [iface (lookup-module-name-in-tenv tenv m-name)])
      (cases interface iface
        (simple-iface (decls)
          (lookup-variable-name-in-decls var-name decls)))))
)

(define lookup-module-name-in-tenv
  (lambda (tenv m-name)
    (let
      ( [maybe-ans (module-name->maybe-binding-in-tenv tenv m-name)])
      (if maybe-ans
        maybe-ans
        (raise-tenv-lookup-failure-error 'module m-name tenv))))
)

(define lookup-variable-name-in-tenv
  (lambda (tenv var-name)
    (let
      ( [maybe-ans (variable-name->maybe-binding-in-tenv tenv var-name)])
      (if maybe-ans
        maybe-ans
        (raise-tenv-lookup-failure-error 'variable var-name tenv))))
)

(define lookup-variable-name-in-decls
  (lambda (search-sym decls0)
    (let loop ( [decls decls0])
      (cond
        ((null? decls) 
          (raise-lookup-variable-in-decls-error! search-sym decls0))
        ((eqv? search-sym (decl->name (car decls)))
          (decl->type (car decls)))
        (else (loop (cdr decls))))))
)

;;! handle error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define raise-lookup-variable-in-decls-error!
  (lambda (var-name decls)
    (eopl:pretty-print
      (list 'lookup-variable-decls-failure:
        (list 'missing-variable var-name)
        'in:
        decls)))
)

(define raise-tenv-lookup-failure-error
  (lambda (kind var tenv)
    (eopl:pretty-print
      (list 'tenv-lookup-failure: (list 'missing: kind var) 'in:
        tenv))
    (eopl:error 'lookup-variable-name-in-tenv))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define variable-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let loop ( [tenv tenv])
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv (var ty saved-tenv)
          (if (eqv? var search-sym)
            ty
            (loop saved-tenv)))
        (else (loop (tenv->saved-tenv tenv))))))
)

(define module-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let loop ( [tenv tenv])
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv-with-module (m-name iface saved-tenv)
          (if (eqv? m-name search-sym)
            iface
            (loop saved-tenv)))
        (else (loop (tenv->saved-tenv tenv))))))
)

(define tenv->saved-tenv
  (lambda (tenv)
    (cases type-environment tenv
      (empty-tenv ()
        (eopl:error 'tenv->saved-tenv
            "tenv->saved-tenv called on empty tenv"))
      (extend-tenv (_ __ saved-tenv)
        saved-tenv)
      (extend-tenv-with-module (_ __ saved-tenv)
        saved-tenv)))
)

(define apply-tenv lookup-variable-name-in-tenv)
