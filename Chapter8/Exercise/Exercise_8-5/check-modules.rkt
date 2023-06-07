#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "static-data-structures.rkt")
(require "checker.rkt")
(require "subtyping.rkt")
(require "expand-type.rkt")

(provide type-of-program)

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (module-defs body)
        (type-of body
          (add-module-defns-to-tenv module-defs (empty-tenv))))))
)

;; add-module-defns-to-tenv : Listof(ModuleDefn) * Tenv -> Tenv
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
      tenv
      (cases module-definition (car defns)
        (a-module-definition (m-name expected-iface m-body)
          (let ( [actual-iface (interface-of m-body tenv)])
            (if (<:-iface actual-iface expected-iface tenv)
              (let
                ( [new-tenv (extend-tenv-with-module m-name expected-iface tenv)])
                (add-module-defns-to-tenv
                  (cdr defns)
                  new-tenv))
              (report-module-doesnt-satisfy-iface m-name
                expected-iface actual-iface)
              ))))
      ))
)

; interface-of : ModuleBody * Tenv -> Iface
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-iface 
          (defns-to-decls defns tenv)))

    (let-module-body (var exp1 body)
      (let ( [tval (type-of exp1 tenv)])
        (interface-of body
          (extend-tenv var tval tenv))))
    
    (letrec-module-body (ty1s p-names b-vars ty2s p-bodies letrec-body)
      (let*
        ( [new-ty1s (map (lambda (ty) (expand-type ty tenv)) ty1s)]
          [new-ty2s (map (lambda (ty) (expand-type ty tenv)) ty2s)]
          [new-ptys (map (lambda (ty1 ty2) (expand-type (proc-type ty2 ty1) tenv)) new-ty1s new-ty2s)]
          [new-tenv (extend-tenv* p-names new-ptys tenv)])
        (check-proc-args! new-ty1s b-vars new-ty2s p-bodies new-tenv)
        (interface-of letrec-body new-tenv)))
    ))
)

; defns-to-decls : Listof(Defn) * Tenv -> Listof(Decl)
(define defns-to-decls
  (lambda (defns tenv)
    (if (null? defns)
      '()
      (cases definition (car defns)
        (val-defn (var exp)
          (let*
            ( [ty (type-of exp tenv)]
              [new-tenv (extend-tenv var ty tenv)])
            (cons
              (val-decl var ty)
              (defns-to-decls (cdr defns) new-tenv)))))))
)

(define report-module-doesnt-satisfy-iface
  (lambda (m-name expected-type actual-type)
    (pretty-print 
      (list 'error-in-defn-of-module: m-name
        'expected-type: expected-type
        'actual-type: actual-type))
    (eopl:error 'type-of-module-defn))
)

(define check-proc-args!
  (lambda (ty1s b-vars ty2s p-bodies tenv)
    (let loop ( [ty1s ty1s] [b-vars b-vars] [ty2s ty2s]
      [p-bodies p-bodies])
      (if (null? ty1s)
        #t
        (let*
          ( [new-tenv (extend-tenv (car b-vars) (car ty2s) tenv)]
            [tval (type-of (car p-bodies) new-tenv)])
          (check-equal-type! tval (car ty1s) (car p-bodies))
          (loop (cdr ty1s) (cdr b-vars) (cdr ty2s) (cdr p-bodies)))  
      )))
)

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp)))
)

;; report-unequal-types : Type * Type * Exp -> Unspecified
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!  
        "Types didn't match: ~s != ~a in~%~a"
        (type-to-external-form ty1)
        (type-to-external-form ty2)
        exp))
)