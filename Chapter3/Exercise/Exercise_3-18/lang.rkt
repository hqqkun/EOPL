#lang eopl

;; grammar for the LET language

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    )
)

; add minus(expr)

(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)

    ;; Exercise 3.18 unpack
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expression
      ("unpack" (arbno identifier) "=" expression "in" expression)
      unpack-exp)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expression
      ("emptylist") empty-list-exp)
    
    (expression
      ("cons" "(" expression "," expression ")")
      cons-exp)

    (expression
      ("car" "(" expression ")")
      car-exp)
    
    (expression
     ("cdr" "(" expression ")")
      cdr-exp)

    (expression
     ("null?" "(" expression ")")
      null?-exp)
    
    (expression 
      ("equal?" "(" expression "," expression ")")
      equal?-exp)
    
    (expression
      ("greater?" "(" expression "," expression ")")
      greater?-exp)

    (expression
      ("less?" "(" expression "," expression ")")
      less?-exp)

    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
     
    (expression
     ("minus" "(" expression ")")
     minus-exp)
    
    (expression
      ("+" "(" expression "," expression ")")
      add-exp)

    (expression
      ("*" "(" expression "," expression ")")
      mul-exp)
    
    (expression
      ("/" "(" expression "," expression ")")
      div-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)
    )
)

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar))
)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar)
)

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar)
)

(provide (all-defined-out))