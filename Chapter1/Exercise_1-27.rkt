#lang eopl

(define flatten
    (lambda (slist)
        (if (null? slist) 
            '()
            (append 
                (flatten-sexp (car slist))
                (flatten (cdr slist)))))
)

(define flatten-sexp
    (lambda (sexp)
        (if (symbol? sexp) 
            (list sexp)
            (flatten sexp)))
)

(define flatten-no-append
    (lambda (slist)
        (letrec
            ([F (lambda (slist tail)
                    (cond
                        ((null? slist) tail)
                        ((symbol? (car slist)) (cons (car slist) (F (cdr slist) tail)))
                        (else (F (car slist) (F (cdr slist) tail))))
                )])
            (F slist '())))
)
; test
(display (flatten-no-append '((a b) c (((d)) e))))