#lang eopl


(define test
  "let count = 0
   in let f = proc(n) -(n, n)
      in begin (f set count = -(count, -1)); count end")

; in call-by-name, test should evaluate to `2`
; in call-by-need, test should evaluate to `1`