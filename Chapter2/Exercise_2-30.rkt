#lang eopl

; 有问题
(define identifier?
  (lambda (x)
    (and (symbol? x) (not (eqv? x 'lambda))))
  )

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?)
   )
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))
   )
)

(define report-invalid-concrete-syntax
  (lambda (cexp errorn)
    (cond
      ((eqv? errorn 1) 
        (eopl:error 
        'parse-expression
        "not a valid expression, exp: ~s" cexp))
      ((eqv? errorn 2) 
        (eopl:error 
          'parse-app-expression 
          "car of app-exp is a symbol, exp : ~s" cexp))
      ((eqv? errorn 3)
        (eopl:error
          'parse-lambda-expression
          "cdr of lambda-exp is null, exp : ~s" cexp))
      ((eqv? errorn 4)
        (eopl:error
          'parse-lambda-expression
          "body of lambda-exp is null, exp : ~s" cexp))))
)

(define cexp->vars cadr)
(define cexp->body caddr)

; cexp -> lc-exp
(define parse-lambda-expression
  (lambda (cexp)
    (cond
      ((null? (cdr cexp))   (report-invalid-concrete-syntax cexp 3))
      ((null? (cddr cexp))  (report-invalid-concrete-syntax cexp 4))
      (else 
        (lambda-exp
          (cexp->vars cexp)
          (parse-expression (cexp->body cexp))))))
)

; cexp -> lc-exp
(define parse-app-expression
  (lambda (cexp)
    (let (
          [rator (parse-expression (car cexp))]
          [rands (map parse-expression (cdr cexp))])
      (cases lc-exp rator
        (var-exp (_) (report-invalid-concrete-syntax cexp 2))
        (else (app-exp rator rands)))))
  )

; cexp -> lc-exp
(define parse-expression
  (lambda (cexp)
    (cond
      ((identifier? cexp) (var-exp cexp))
      ((pair? cexp)
       (if (eqv? (car cexp) 'lambda)
           (parse-lambda-expression cexp)
           (parse-app-expression cexp)))
      (else (report-invalid-concrete-syntax cexp 1))))      
)

; test
(parse-expression '(lambda (x y)))