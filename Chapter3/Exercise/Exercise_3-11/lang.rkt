#lang eopl

;; grammar for the LET language

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (binary-rator 
      ((or "cons" "+" "-" "*" "/" "equal?" "greater?" "less?")) string)
    (unary-rator
      ((or "minus" "zero?" "null?" "car" "cdr")) string)
    (arbno-rator
      ("list") string)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    )
)

; add minus(expr)

(define the-grammar
  '((program (expression) a-program)

    ;; Exercise 3.11
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expression
      (binary-rator "(" expression "," expression ")") 
      binary-exp)
    
    (expression
      (unary-rator "(" expression ")")
      unary-exp)
    
    (expression
      (arbno-rator "(" (separated-list expression ",") ")")
      arbno-exp)

    (expression (number) const-exp)

    (expression
      ("emptylist") empty-list-exp)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
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