#lang eopl

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) 
        (value-of exp1 (init-env)))))
)

; value-of-explist : 
;   ListOf(Expression) * Environment -> ExpVal
; for ExpVal, only `empty-val` or `pair-val`
(define value-of-explist
  (lambda (lst env)
    (letrec
      ([V (lambda (lst)
            (if (null? lst)
              (empty-val)
              (let 
                ( [car-val (value-of (car lst) env)]
                  [cdr-val (V (cdr lst))])
                (pair-val car-val cdr-val))))])
    (V lst)))
)

; value-of : Expression * Environment -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      ; Exercise 3.11
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (binary-exp (rator exp1 exp2)
        (if (num-rator? rator)
          (let*
            ( [w (wrapper rator)]
              [make (car w)]
              [func (cadr w)]
              [num1 (expval->num (value-of exp1 env))]
              [num2 (expval->num (value-of exp2 env))])
            (make (func num1 num2)))
          (let*
            ( [w (wrapper rator)]
              [make (car w)]
              [val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)])
            (make val1 val2))))

      (unary-exp (rator exp1)
        (if (num-rator? rator)
          (let*
            ( [w (wrapper rator)]
              [make (car w)]
              [func (cadr w)]
              [num (expval->num (value-of exp1 env))])
            (make (func num)))
          (let*
            ( [w (wrapper rator)]
              [make (car w)]
              [val1 (value-of exp1 env)])
            (if (null? (cdr w))
              (make val1)
              (make ((cadr w) val1))
            ))))

      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))     

      (arbno-exp (_ lst)
        (value-of-explist lst env))

      (empty-list-exp () (empty-val))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)))

      (let-exp (var exp1 body)
        (let*
          ( [val (value-of exp1 env)]
            [new-env (extend-env var val env)])
          (value-of body new-env)))
    ))
)

(define num-rator?
  (lambda (rator)
    (cond
      ((equal? rator "cons") #f)
      ((equal? rator "car") #f)
      ((equal? rator "cdr") #f)
      ((equal? rator "null?") #f)
      (else #t)))
)

(define wrapper
  (lambda (rator)
    (cond
      ((equal? rator "+") (list num-val +))
      ((equal? rator "-") (list num-val -))
      ((equal? rator "*") (list num-val *))
      ((equal? rator "/") (list num-val quotient))
      ((equal? rator "equal?") (list bool-val =))
      ((equal? rator "greater?") (list bool-val >))
      ((equal? rator "less?") (list bool-val <))
      ((equal? rator "minus") (list num-val -))
      ((equal? rator "zero?") (list bool-val zero?))
      ((equal? rator "cons") (list pair-val))
      ((equal? rator "null?") (list bool-val expval->null?))
      ((equal? rator "car") (list expval->pair->first))
      ((equal? rator "cdr") (list expval->pair->second))
      (else (eopl:error 'wrapper "no operator for ~s" rator)))
    )
)

(provide value-of-program value-of)