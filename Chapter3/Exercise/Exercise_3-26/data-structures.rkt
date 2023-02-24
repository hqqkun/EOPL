#lang eopl

(require "lang.rkt")
(require "lang.rkt")
(require racket/set)

(define identifier? symbol?)
;;;;;;;;;;;;;;;; proc value ;;;;;;;;;;;;;;;;
(define-datatype proc proc?
  (procedure
    (var identifier?)
    (body expression?)
    (saved-env environment?))
)

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
   (proc proc?))
)

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
      (else (report-expval-extractor-error 'proc val))))
)

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val))))
)

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val))))
)


(define report-expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value))
)


;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (var symbol?)
    (val expval?)
    (old-env environment?))
)



;;;;;;;;;;;;;;;; set operations ;;;;;;;;;;;;;;;;

; free-vars-of-exp : 
;   Expression * Setof(BoundVar)
;     -> Setof(freeVar)
(define free-vars-of-exp
  (lambda (exp bound-set)
    (cases expression exp
      (const-exp (_) (set))
      (diff-exp (exp1 exp2)
        (let
          ( [set1 (free-vars-of-exp exp1 bound-set)]
            [set2 (free-vars-of-exp exp2 bound-set)])
          (set-union set1 set2)))

      (if-exp (exp1 exp2 exp3)
        (let 
          ( [set1 (free-vars-of-exp exp1 bound-set)]
            [set2 (free-vars-of-exp exp2 bound-set)]
            [set3 (free-vars-of-exp exp3 bound-set)])
          (set-union set1 set2 set3)))

      (zero?-exp (exp1) (free-vars-of-exp exp1 bound-set))

      (let-exp (var exp1 body)
        (let
          ( [set1 (free-vars-of-exp exp1 bound-set)]
            [set2 (free-vars-of-exp body (set-add bound-set var))])
          (set-union set1 set2)))

      (var-exp (var) 
        (if (set-member? bound-set var) 
          (set)
          (set var)))

      (proc-exp (var body)
        (free-vars-of-exp body (set-add bound-set var)))

      (call-exp (exp1 exp2)
        (let
          ( [set1 (free-vars-of-exp exp1 bound-set)]
            [set2 (free-vars-of-exp exp2 bound-set)])
          (set-union set1 set2)))))
)

(define free-vars-inproc
  (lambda (var body)
    (free-vars-of-exp body (set var))
  )
)

(provide (all-defined-out)) 