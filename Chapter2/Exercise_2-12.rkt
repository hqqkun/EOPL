#lang eopl

(define empty-stack
  (lambda ()
    (lambda (cmd)
      (cond
        ((eqv? cmd 'pop) (eopl:error 'empty-stack "stack is empty.~%"))
        ((eqv? cmd 'top) (eopl:error 'empty-stack "stack is empty.~%"))
        ((eqv? cmd 'empty?) #t))))
)

(define empty-stack?
  (lambda (stack)
    (stack 'empty?))
)

(define pop
  (lambda (stack)
    (stack 'pop))
)

(define top
  (lambda (stack)
    (stack 'top))
)

(define push
  (lambda (item stack)
    (lambda (cmd)
      (cond
        ((eqv? cmd 'pop) stack)
        ((eqv? cmd 'top) item)
        ((eqv? cmd 'empty?) #f))))
)

; test
(define stack (push 98 (push 45 (empty-stack))))
(display (top (pop stack)))