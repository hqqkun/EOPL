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
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))

      (minus-exp (exp1) 
        (num-val 
          (- (expval->num (value-of exp1 env)))))
        
      ; Exercise 3.10
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (make-list-exp (lst)
        (value-of-explist lst env))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (empty-list-exp () (empty-val))

      (cons-exp (exp1 exp2)
        (let 
          ( [exp1-val (value-of exp1 env)]
            [exp2-val (value-of exp2 env)])
        (pair-val exp1-val exp2-val)))

      (car-exp (exp1)
        (expval->pair->first (value-of exp1 env)))
      
      (cdr-exp (exp1)
        (expval->pair->second (value-of exp1 env)))

      (null?-exp (exp1)
        (let ([exp1-val (value-of exp1 env)])
          (bool-val (expval->null? exp1-val))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (equal?-exp (exp1 exp2)
        (let 
          ( [num1 (expval->num (value-of exp1 env))]
            [num2 (expval->num (value-of exp2 env))])
          (bool-val (= num1 num2))))

      (greater?-exp (exp1 exp2)
        (let 
          ( [num1 (expval->num (value-of exp1 env))]
            [num2 (expval->num (value-of exp2 env))])
          (bool-val (> num1 num2))))

      (less?-exp (exp1 exp2)
        (let 
          ( [num1 (expval->num (value-of exp1 env))]
            [num2 (expval->num (value-of exp2 env))])
          (bool-val (< num1 num2))))
        
      (diff-exp (exp1 exp2)
        (num-val 
          (-  (expval->num (value-of exp1 env))
              (expval->num (value-of exp2 env)))))
      (add-exp (exp1 exp2)
        (num-val
          (+ (expval->num (value-of exp1 env))
             (expval->num (value-of exp2 env)))))

      (mul-exp (exp1 exp2)
        (num-val 
          (* (expval->num (value-of exp1 env))
             (expval->num (value-of exp2 env)))))

      (div-exp (exp1 exp2)
        (num-val
          (quotient (expval->num (value-of exp1 env))
                    (expval->num (value-of exp2 env)))))
        
      (zero?-exp (exp1)
        (let* ( [val (value-of exp1 env)]
                [num (expval->num val)])
          (if (zero? num) 
            (bool-val #t)
            (bool-val #f))))

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

(provide value-of-program value-of)