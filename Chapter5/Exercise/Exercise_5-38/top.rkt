#lang eopl

;; top level module.  Loads all required pieces.
;; Run the test suite with (run-all).

(require "drscheme-init.rkt")
(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interpreter.rkt")      ; for value-of-program
(require "tests.rkt")            ; for test-list

(provide (all-defined-out))
(provide (all-from-out "interpreter.rkt"))
(provide (all-from-out "lang.rkt"))

;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; run-all : () -> Unspecified

;; runs all the tests in test-list, comparing the results with
;; equal-answer?  

(define run-all
  (lambda ()
    (run-tests! run equal-answer? test-list)))

(define equal-answer?
  (lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval 
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      ((list? sloppy-val) (list-val (map sloppy->expval sloppy-val)))
      (else
        (eopl:error 'sloppy->expval 
                    "Can't convert sloppy value to expval: ~s"
                    sloppy-val)))))
  
;; run-one : Sym -> ExpVal

;; (run-one sym) runs the test whose name is sym

(define run-one
  (lambda (test-name)
    (let ((the-test (assoc test-name test-list)))
      (cond
        ((assoc test-name test-list)
          => (lambda (test)
              (run (cadr test))))
        (else (eopl:error 'run-one "no such test: ~s" test-name))))))

(run-all)