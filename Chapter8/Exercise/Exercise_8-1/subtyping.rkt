#lang eopl

(require "lang.rkt")

(provide <:-iface)

(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases interface iface1
      (simple-iface (decls1)
        (cases interface iface2
          (simple-iface (decls2)
            (<:-decls decls1 decls2 tenv))))))
)

; decls1 = act
; decls2 = exp
(define <:-decls 
  (lambda (decls1 decls2 tenv)
    (cond
      ((null? decls2) #t)
      ((null? decls1) #f)
      (else (let
        ( [name1 (decl->name (car decls1))]
          [name2 (decl->name (car decls2))]
          [type1 (decl->type (car decls1))]
          [type2 (decl->type (car decls2))])
        (if 
          (and 
            (eqv? name1 name2)
            (equal? type1 type2))
          (<:-decls (cdr decls1) (cdr decls2) tenv)
          (<:-decls (cdr decls1) decls2 tenv))))  
    ))
)
