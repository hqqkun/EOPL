#lang eopl

; stack
(define-datatype stack stack?
  (empty-stack)
  (not-empty-stack
    (top always?)
    (rest stack?))
)

(define empty-stack?
  (lambda (stk)
    (cases stack stk
      (empty-stack () #t)
      (else #f)))
)

(define top
  (lambda (stk)
    (cases stack stk
      (empty-stack () (report-stack-error 'top "stack is empty.~%"))
      (not-empty-stack (top _) top)))
)

(define pop
  (lambda (stk)
    (cases stack stk 
      (empty-stack () (report-stack-error 'pop "stack is empty.~%"))
      (not-empty-stack (_ rest) rest)))
)

(define push
  (lambda (item stk)
    (cases stack stk
      (else (not-empty-stack item stk))))
)


(define report-stack-error
  (lambda (which-func msg)
    (eopl:error which-func msg))
)

; test
(define stk (push 12 (push 45 (empty-stack))))
(display (pop (pop (pop stk))))
