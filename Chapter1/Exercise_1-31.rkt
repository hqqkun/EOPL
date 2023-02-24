#lang eopl

; Bintree ::= Int | (Symbol Bintree Bintree)
(define leaf (lambda (x) x))
(define interior-node
    (lambda (sym left right)
        (list sym left right)
    )
)

(define leaf? integer?)
(define lson cadr)
(define rson caddr)

(define contents-of
  (lambda (bintree)
    (if (leaf? bintree)
        bintree
        (car bintree)))
)

; test
(provide (all-defined-out))