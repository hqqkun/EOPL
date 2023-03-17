#lang eopl

(require "drscheme-init.rkt")
(require "store.rkt")

(provide (all-defined-out))

(define-datatype array array?
  (an-array
    (s-index reference?)
    (len integer?))
)
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
          (an-array start-index length)))))
)

(define arrayref
  (lambda (arr index)
    (cases array arr
      (an-array (s-index len)
        (if (> index (- len 1))
          (eopl:error "false : index!")
          (deref (+ s-index index))))))
)

(define arrayset
  (lambda (arr index val)
    (cases array arr
      (an-array (s-index len)
        (if (> index (- len 1))
          (eopl:error "false : index!")
          (setref! (+ s-index index) val)))))
)

(define length-of-arr
  (lambda (arr)
    (cases array arr
      (an-array (s-index len)
        len)))
)