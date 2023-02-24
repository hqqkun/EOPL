#lang eopl


; Binary-search-tree ::= () | (Int Binary-search-tree Binary-search-tree)

(define lson cadr)
(define rson caddr)
(define contents-of car)

; assume bst has n
; Int * bst -> los
(define path
    (lambda (n bst)
        (let 
            ([num (contents-of bst)]
             [left (lson bst)]
             [right (rson bst)]
            )
            (cond
                ((= n num) '())
                ((< n num) (cons 'left (path n left)))
                (else (cons 'right (path n right))))))
)

; test
(display 
    (path 17 '(14   (7 () (12 () ()))
                    (26 (20 (17 () ())
                            ())
                        (31 () ())))))