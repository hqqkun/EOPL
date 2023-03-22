#lang eopl

; the original program in exercise 3.25 works fine under call-by-need.
; since it's used in call-by-value, then everything in it can be evaluated as a `value`.
; then call-by-need works find as well.

; but the program written in `cll-by-need` won't work on `call-by-value`. Since `(d d)` will
; never return