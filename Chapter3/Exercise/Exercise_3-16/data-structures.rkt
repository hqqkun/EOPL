#lang eopl

; (define-datatype program program?
;   (a-program
;     (exp1 expression?))
; )

;;;;;;;;;;;;;;;; expression ;;;;;;;;;;;;;;;;
; (define identifier? symbol?)

; (define-datatype expression expression?
;   (const-exp
;     (num number?))
;   (diff-exp
;     (exp1 expression?)
;     (exp2 expression?))
;   (zero?-exp
;     (exp1 expression?))
;   (if-exp
;     (exp1 expression?)
;     (exp2 expression?)
;     (exp3 expression?))
;   (var-exp
;     (var identifier?))
;   (let-exp
;     (var identifier?)
;     (exp1 expression?)
;     (body expression?))
; )

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
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