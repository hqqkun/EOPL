#lang eopl

(require "drscheme-init.rkt")
(require "store.rkt")

(provide (all-defined-out))

(define array? reference?)

; make-array : Length * Expval -> Array
(define make-array 
  (lambda (length val)
    (letrec
      ( [M  (lambda (length)
              (if (zero? length)
                #t
                (begin
                (newref val)
                (M (- length 1)))))])
      (let
        ( [start-index (newref val)])
        (begin
          (M (- length 1))
          start-index))))
)

(define arrayref
  (lambda (arr index)
    (deref (+ arr index)))
)

(define arrayset
  (lambda (arr index val)
    (setref! (+ arr index) val))
)