#lang eopl

(require "lang.rkt")
(require racket/base)

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
    (let 
      ( [table (make-actual-iface-decls-hash decls1)])
      (let loop ( [decls2 decls2])
        (cond
          ((null? decls2) #t)
          ((hash-has-key? table (decl->name (car decls2)))
            (if (equal?
                  (hash-ref table (decl->name (car decls2)))
                  (decl->type (car decls2)))
              (loop (cdr decls2))
              #f))
          (else #f)))))
)

(define make-actual-iface-decls-hash
  (lambda (decls)
    (let 
      ( [table (make-hash)])
      (let loop ([decls decls])
        (if (null? decls)
          table
          (begin
            (hash-set! table 
              (decl->name (car decls))
              (decl->type (car decls)))
            (loop (cdr decls)))))))
)
