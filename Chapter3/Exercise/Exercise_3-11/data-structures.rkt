#lang eopl

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (empty-val)
  (pair-val
    (first expval?)
    (second expval?))
)

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
      (else (report-expval-extractor-error 'pair val))))
)

(define expval->pair->second
  (lambda (val)
    (cases expval val
      (pair-val (_ second) second)
      (else (report-expval-extractor-error 'pair val))))
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