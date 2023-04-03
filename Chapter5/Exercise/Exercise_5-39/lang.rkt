#lang eopl               

;; grammar for the LETREC language

(require "drscheme-init.rkt")

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
  '((program (expression) a-program)

    ; new staff
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expression
      ("try" expression "catch" "(" identifier ")" expression)
      try-exp)

    (expression
      ("raise" expression)
      raise-exp)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; list
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expression
      ("list" "(" (separated-list number ",") ")")
      const-list-exp)
    (expression
      (unary-op "(" expression ")")
      unop-exp)
    (unary-op 
      ("null?") null?-unop)
    (unary-op 
      ("car") car-unop)
    (unary-op 
      ("cdr") cdr-unop)
    (unary-op
      ("zero?") zero?-unop)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (expression (number) const-exp)
    (expression
      ("-" "(" expression "," expression ")")
      diff-exp)

    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)

    (expression (identifier) var-exp)

    (expression
      ("let" identifier "=" expression "in" expression)
      let-exp)   

    (expression
      ("proc" "(" identifier ")" expression)
      proc-exp)

    (expression
      ("(" expression expression ")")
      call-exp)

    (expression
      ("letrec"
        identifier "(" identifier ")" "=" expression
          "in" expression)
      letrec-exp)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
