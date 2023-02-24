#lang eopl
(define empty-stack
    (lambda () '())
)

(define empty-stack? null?)

(define push
    (lambda (item stack)
        (cons item stack))
)

(define pop cdr)

(define top car)

; test
(define stk (push 12 (push 45 (empty-stack))))
(display stk)