#lang eopl

(require "drscheme-init.rkt")
(require scheme/base)

(provide initialize-store! reference? newref deref setref!
         instrument-newref get-store-as-list)

(define instrument-newref (make-parameter #f))

;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;

;;; world's dumbest model of the store:  the store is a list and a
;;; reference is number which denotes a position in the list.

;; the-store: a Scheme variable containing the current state of the
;; store.  Initially set to a dummy variable.
(define the-store 'uninitialized)

; static variable
(define next-index 'uninitialized)

;; empty-store : () -> Sto
;; Page: 111
(define empty-store
  (lambda () (make-vector 1)))

;; initialize-store! : () -> Sto
;; usage: (initialize-store!) sets the-store to the empty-store
;; Page 111
(define initialize-store!
  (lambda ()
    (set! next-index 0)
    (set! the-store (empty-store))))

;; get-store : () -> Sto
;; Page: 111
;; This is obsolete.  Replaced by get-store-as-list below
(define get-store
  (lambda () the-store))

;; reference? : SchemeVal -> Bool
;; Page: 111
(define reference?
  (lambda (v)
    (integer? v)))

;; newref : ExpVal -> Ref
;; Page: 111
(define newref
  (lambda (val)
    (begin
      (if (< next-index (vector-length the-store))
        #t
        (let
          ( [new-store (make-vector (* 2 (vector-length the-store)))])

          (vector-copy! new-store 0 the-store)
          (set! the-store new-store)))

      (vector-set! the-store next-index val)
      (let ([old-index next-index])
        (set! next-index (+ next-index 1))
        old-index)))
)

;; deref : Ref -> ExpVal
;; Page 111
(define deref
  (lambda (ref)
    (vector-ref the-store ref)))

;; setref! : Ref * ExpVal -> Unspecified
;; Page: 112
(define setref!
  (lambda (ref val)
    (vector-set! the-store ref val))
)

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))

;; get-store-as-list : () -> Listof(List(Ref,Expval))
;; Exports the current state of the store as a scheme list.
;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
;;   where foo, bar, and baz are expvals.
;; If the store were represented in a different way, this would be
;; replaced by something cleverer.
;; Replaces get-store (p. 111)
(define get-store-as-list
  (lambda ()
    (letrec
        ((inner-loop
          ;; convert sto to list as if its car was location n
          (lambda (sto n)
            (if (null? sto)
                '()
                (cons
                 (list n (car sto))
                 (inner-loop (cdr sto) (+ n 1)))))))
      (inner-loop the-store 0))))
