#lang eopl

(define-datatype continuation continuation?
  (end-cont)
  (remove-cont
    (item symbol?)
    (cont continuation?))
)



(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf "End of computation.~%")
          (eopl:printf "This sentence should appear only once.~%")
          val))
      (remove-cont (item saved-cont)
        (apply-cont saved-cont (cons item val)))))
)

; continuation version

(define remove-first
  (lambda (s los)
    (remove-first/k s los (end-cont)))
)

(define remove-first/k
  (lambda (s los cont)
    (cond
      ((null? los) (apply-cont cont '()))
      ((eqv? (car los) s) (apply-cont cont (cdr los)))
      (else 
        (remove-first/k s (cdr los)
          (remove-cont (car los) cont)))))
)

(display (remove-first 'b '(e f g)))

