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

    ;; Exercise 3.14  change if
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expression
      (bool-expression)
      bool-exp)
    (expression 
      ("if" bool-expression "then" expression "else" expression)
      if-exp)

    (bool-expression
      ("zero?" "(" expression ")")
      zero?-exp)
    
    (bool-expression 
      ("equal?" "(" expression "," expression ")")
      equal?-exp)
    
    (bool-expression
      ("greater?" "(" expression "," expression ")")
      greater?-exp)
    
    (bool-expression
      ("less?" "(" expression "," expression ")")
      less?-exp)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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