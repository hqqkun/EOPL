#lang eopl

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '(
    (type
     ("int")
     int-type)

    (type
     ("bool")
     bool-type)

    (type
     ("(" type "->" type ")")
     proc-type)

    (type
      ("tvar-type" number)
      tvar-type)
    )
)

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
        (list
          (type-to-external-form arg-type)
          '->
          (type-to-external-form result-type)))
      (tvar-type (num)
        (string->symbol
          (string-append "tvar_"
            (number->string num))))
    ))        
)

;;;;;;;;;;;;;;;; syntactic tests and observers ;;;;;;;;;;;;;;;;
(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type)
        #t)
      (else #f)))
)

(define tvar-type?
  (lambda (ty)
    (cases type ty
      (tvar-type (sn) #t)
      (else #f)))
)

(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type _)
        arg-type)
      (else 
        (eopl:error 
          'proc-type->arg-type
          "Not a proc type: ~s" ty))))
)

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (_ result-type)
        result-type)
      (else 
        (eopl:error 
          'proc-type->result-type
          "Not a proc type: ~s" ty))))
)
