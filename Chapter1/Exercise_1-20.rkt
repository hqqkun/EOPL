#lang eopl

(define count-occurrences
    (lambda (s slist)
        (apply 
            +
            (map 
                (lambda (s-exp)
                    (if (symbol? s-exp)
                        (if (eqv? s-exp s) 1 0)
                        (count-occurrences s s-exp)))
                slist)))
)

; test
(display (count-occurrences 'x '((f x) y (((x z) x)))))
(newline)
(display (count-occurrences 'x '((f x) y (((x z) () x)))))
(newline)
(display (count-occurrences 'w '((f x) y (((x z) x)))))