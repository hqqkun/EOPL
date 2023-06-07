#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "static-data-structures.rkt")
(require "checker.rkt")
(require "subtyping.rkt")

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
        (a-module-definition (m-name expected-iface sub-defns m-body)
          (let*
            ( [new-tenv (add-module-defns-to-tenv sub-defns tenv)]
              [actual-iface (interface-of m-body new-tenv)])
            (if (<:-iface actual-iface expected-iface new-tenv)
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
          (defns-to-decls defns tenv)))))
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
