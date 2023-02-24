#lang eopl

(require "lang.rkt")

;;;;;;;;;;;;;;;; proc value ;;;;;;;;;;;;;;;;
(define-datatype proc proc?
  (trace-procedure
    (var symbol?)
    (body expression?)
    (saved-env environment?))
  
  (procedure
    (var symbol?)
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

(provide (all-defined-out)) 