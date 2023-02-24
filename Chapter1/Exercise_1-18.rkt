#lang eopl

; slist -> slist
(define swapper
    (lambda (s1 s2 slist)
        (map 
            (lambda (s-exp)
                (if (symbol? s-exp)
                    (cond
                        ((eqv? s-exp s1) s2)
                        ((eqv? s-exp s2) s1)
                        (else s-exp))
                    (swapper s1 s2 s-exp)))
            slist))
)

; test
(display (swapper 'a 'd '(a b c d)))
(newline)
(display (swapper 'a 'd '(a d () c d)))
(newline)
; (d a () c a)
(display (swapper 'x 'y '((x) y (z (x)))))
; ((y) x (z (y)))