#lang eopl

(require "drscheme-init.rkt")
(require "store.rkt")

(provide (all-defined-out))

(define mutpair? reference?)

(define make-pair 
  (lambda (val1 val2)
    (let*
      ( [ref1 (newref val1)]
        [ref2 (newref val2)])
      ref1))
)

(define left
  (lambda (p)
    (deref p))
)

(define right
  (lambda (p)
    (deref (+ p 1)))
)

(define setleft
  (lambda (p val)
    (setref! p val))
)

(define setright
  (lambda (p val)
    (setref! (+ p 1) val))
)
