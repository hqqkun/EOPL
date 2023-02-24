#lang eopl

; Bintree ::= Int | (Symbol Bintree Bintree)
(require "Exercise_1-31.rkt")

(define number-leaves
    (lambda (bintree)
        (car (helper bintree 0)))
)

; bintree * Int -> (cons bintree, Int)
; return the modified tree and next_index
(define helper
    (lambda (bintree index)
        (if (leaf? bintree)
            (cons (leaf index) (+ index 1))
            (let* 
                ([sym (contents-of bintree)]
                 [left-res (helper (lson bintree) index)]
                 [left (car left-res)]
                 [next-index (cdr left-res)]
                 [right-res (helper (rson bintree) next-index)]
                 [right (car right-res)]
                 [res-index (cdr right-res)]
                )
                (cons (interior-node sym left right) res-index))))
)

; test
(define tree
    (interior-node 'foo
        (interior-node 'bar
            (leaf 26)
            (leaf 12))
        (interior-node 'baz
            (leaf 11)
            (interior-node 'quux
                (leaf 117)
                (leaf 14))))
)

(display (number-leaves tree))