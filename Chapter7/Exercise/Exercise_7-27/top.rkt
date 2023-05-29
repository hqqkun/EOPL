#lang eopl

;; top level module.  Loads all required pieces.
;; Run the test suite for the interpreter with (run-all).
;; Run the test suite for the checker with (check-all).

(require "drscheme-init.rkt")
(require "data-structures.rkt")       ; for expval constructors
(require "lang.rkt")                  ; for scan&parse
(require "inferrer.rkt")              ; for type-of-program
(require "tests.rkt")                 ; for test-for-run and tests-for-check

(require "equal-up-to-gensyms.rkt")   ; for equal-up-to-gensyms

(provide check check-all)

;;; interface for book test ;;;
(provide test-all)
(define (test-all) (check-all) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check : string -> external-type

(define check
  (lambda (string)
    (type-to-external-form
      (type-of-program (scan&parse string)))))

;; check-all : () -> unspecified
;; checks all the tests in test-list, comparing the results with
;; equal-types?

(define check-all
  (lambda ()
    (run-tests! check equal-types? tests-for-check)))

;; check-one : symbol -> expval
;; (check-one sym) checks the test whose name is sym

(define check-one
  (lambda (test-name)
    (let ((the-test (assoc test-name tests-for-check)))
      (cond
        (the-test
          => (lambda (test)
              (check (cadr test))))
        (else (eopl:error 'check-one "no such test: ~s" test-name))))))

(stop-after-first-error #t)

(check-all)
