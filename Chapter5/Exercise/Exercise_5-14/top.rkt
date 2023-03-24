#lang eopl

;; top level module.  Loads all required pieces.
;; Run the test suite with (run-all).

(require "drscheme-init.rkt")
(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interpreter.rkt")      ; for value-of-program

(provide (all-defined-out))
(provide (all-from-out "interpreter.rkt"))
(provide (all-from-out "lang.rkt"))

;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define fact-rec
  "letrec 
    fact(x) = if zero?(x) then 1 else *(x, (fact -(x, 1)))
   in (fact 8)")

(define fact-iter
  "letrec fact-iter-acc(n, a) = 
            if zero?(n) then a else
            (fact-iter-acc -(n, 1) *(n, a))
   in let fact-iter = proc(n) (fact-iter-acc n 1)
      in (fact-iter 8)")
  
(run fact-iter)