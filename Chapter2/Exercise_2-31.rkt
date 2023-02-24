#lang eopl

(define-datatype prefix-exp prefix-exp?
  (const-exp
    (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?))
)

; List -> prefix-exp
(define parse-prefix-list
  (lambda (prefix-list)
    (let
      ([res (parse-prefix-exp prefix-list)])
      (car res)))
)

; helper function
; List -> (Prefix-exp, Restof(List))
(define parse-prefix-exp
  (lambda (prefix-list)
    (cond
      ((null? prefix-list) (cons '() '()))
      ((integer? (car prefix-list)) (cons (const-exp (car prefix-list)) (cdr prefix-list)))
      (else 
        (let* 
          ( [left-res (parse-prefix-exp (cdr prefix-list))]
            [operand1 (car left-res)]
            [new-list (cdr left-res)]
            [right-res (parse-prefix-exp new-list)]
            [operand2 (car right-res)]
            [rest-list (cdr right-res)])
          (cons (diff-exp operand1 operand2) rest-list)))))
)

; test
(define exp '(- - 3 2 - 4 - 12 7))
(define compare-exp 
        (diff-exp
          (diff-exp
            (const-exp 3)
            (const-exp 2))
          (diff-exp             
            (const-exp 4)
            (diff-exp
              (const-exp 12)
              (const-exp 7)))))

(define dif-exp (parse-prefix-list exp))
(display (equal? dif-exp compare-exp))