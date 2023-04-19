#lang eopl

(define end-cont
  (lambda ()
    (lambda (val)
       (begin
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        val)))
)

(define occurs-free?/k
  (lambda (var exp cont)
    (cond
      ((symbol? exp) (cont (eqv? var exp)))
      ((eqv? (car exp) 'lambda)
        (occurs-free?/k var (caddr exp)
          (lambda (val)
            (cont (and (not (eqv? var (caadr exp))) val)))))
      (else
        (occurs-free?/k var (car exp)
          (lambda (val1)
            (occurs-free?/k var (cadr exp)
              (lambda (val2)
                (cont (or val1 val2)))))))
        ))
)

(define occurs-free?
  (lambda (var exp)
    (occurs-free?/k var exp (end-cont)))
)

(display (occurs-free? 'x '(lambda (x) (x y))))
