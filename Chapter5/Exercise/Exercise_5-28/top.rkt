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

(define str "-(-(44,11),3)")
(run str)