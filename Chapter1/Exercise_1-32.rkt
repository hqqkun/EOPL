#lang eopl

; Bintree ::= Int | (Symbol Bintree Bintree)
(require "Exercise_1-31.rkt")

(define double-tree
    (lambda (bintree)
        (if (leaf? bintree)
            (leaf (* 2 (contents-of bintree)))
            (interior-node 
                (contents-of bintree)
                (double-tree (lson bintree))
                (double-tree (rson bintree)))))
)

; test
(define tree (interior-node 'x (leaf 10) (leaf 20)))
(display (double-tree tree))