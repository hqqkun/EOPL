#lang eopl

(require "lang.rkt")                  ; for expression?

(provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (boolean boolean?))
  (proc-val 
    (proc proc?))
  (empty-val)
  (pair-val
    (first expval?)
    (second expval?)))

;;; extractors:

; ExpVal -> Bool
; if expval is a empty list, then true
(define expval->null?
  (lambda (val)
    (cases expval val
      (empty-val () #t)
      (else #f)))
)

(define expval->pair->first
  (lambda (val)
    (cases expval val
      (pair-val (first _) first)
      (else (expval-extractor-error 'pair val))))
)

(define expval->pair->second
  (lambda (val)
    (cases expval val
      (pair-val (_ second) second)
      (else (expval-extractor-error 'pair val))))
)

(define expval->num
  (lambda (v)
    (cases expval v
  (num-val (num) num)
  (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
  (bool-val (bool) bool)
  (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
  (proc-val (proc) proc)
  (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
  variant value)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

;; Page: 148
(define identifier? symbol?)

(define-datatype continuation continuation?
  (end-cont)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (make-pair-cont
    (first-val expval?)
    (saved-cont continuation?))
  (rest-cont
    (exps (list-of expression?))
    (saved-env environment?)
    (saved-cont continuation?))
  (car-cont
    (saved-cont continuation?)) 
  (null-cont
    (saved-cont continuation?))
  (cdr-cont
    (saved-cont continuation?))
  (cons-1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (cons-2-cont
    (val1 expval?)
    (saved-cont continuation?))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (zero1-cont
    (saved-cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (if-test-cont 
    (exp2 expression?)
    (exp3 expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (diff1-cont                
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (diff2-cont                
    (val1 expval?)
    (saved-cont continuation?))
  (rator-cont            
    (rand expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (rand-cont             
    (val1 expval?)
    (saved-cont continuation?)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
    (bvar symbol?)
    (body expression?)
    (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env)
  (extend-env 
    (bvar symbol?)
    (bval expval?)
    (saved-env environment?))
  (extend-env-rec
    (p-name symbol?)
    (b-var symbol?)
    (p-body expression?)
    (saved-env environment?)))