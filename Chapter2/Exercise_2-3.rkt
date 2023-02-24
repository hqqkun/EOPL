#lang eopl

; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)

(define zero
    (lambda ()
        '(diff (one) (one)))
)

(define one
    (lambda ()
        (make-diff-tree '(one) (zero)))
)

(define minus-one
    (lambda ()
        (make-diff-tree (zero) '(one)))
)

(define lson cadr)
(define rson caddr)

(define make-diff-tree
    (lambda (left right)
        (list 'diff left right))
)

(define diff-tree-value
    (lambda (diff-tree)
        (if (eqv? (car diff-tree) 'one)
            1
            (let 
                ([left-val (diff-tree-value (lson diff-tree))]
                 [right-val (diff-tree-value (rson diff-tree))]
                )
                (- left-val right-val))))
)

(define is-zero?
    (lambda (diff-tree)
        (= (diff-tree-value diff-tree) 0))
)

(define successor
    (lambda (diff-tree)
        (make-diff-tree diff-tree (minus-one)))
)

(define predecessor
    (lambda (diff-tree)
        (make-diff-tree diff-tree (one)))
)

(define diff-tree-plus
    (lambda (a b)
        (make-diff-tree
            a
            (make-diff-tree (zero) b)))
)
; test
(define num '(diff (one) (diff (one) (one))))
(define add-1-num (successor num))
(define num_plus (diff-tree-plus num add-1-num))
(display (diff-tree-value num_plus))