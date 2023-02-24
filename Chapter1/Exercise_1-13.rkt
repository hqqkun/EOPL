#lang eopl 

(define subset-map
    (lambda (new old slist)
        (map 
            (lambda (s-exp) (subset-in-s-exp new old s-exp)) 
            slist))
)

(define subset-in-s-exp
    (lambda (new old s-exp)
        (if (symbol? s-exp)
            (if (eqv? s-exp old) new s-exp)
            (subset new old s-exp)))
)

(define subset subset-map)

; test
(define s-list '((b c) (b () d)))
(display (subset 'a 'b s-list))