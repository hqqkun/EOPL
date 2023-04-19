#lang eopl

(define end-cont
  (lambda (val)
    (begin
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val))
)

(define subst
  (lambda (new old slist)
    (subst/k new old slist end-cont))
)


(define subst-in-s-exp
  (lambda (new old sexp cont)
    (if (symbol? sexp)
      (if (eqv? old sexp) 
        (cont new)
        (cont sexp))
      (subst/k new old sexp cont)))
)

(define subst/k
  (lambda (new old slist cont)
    (if (null? slist)
      (cont '())
      (subst/k new old (cdr slist)
        (lambda (val-rest)
          (subst-in-s-exp new old (car slist) 
            (lambda (val-first)
              (cont (cons val-first val-rest))))))))
)


(display (subst 'a 'b '((b c) (b () d))))