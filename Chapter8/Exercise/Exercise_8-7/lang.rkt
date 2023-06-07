#lang eopl             

;; grammar for simple modules
;; based on CHECKED.

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
    
    (program
      ((arbno module-definition)
        expression)
      a-program)

    (module-definition
      ("module" identifier 
        "interface" interface
        "body" 
        (arbno module-definition)
        module-body)
      a-module-definition)


    (interface
      ("[" (arbno declaration) "]") 
      simple-iface)


    (declaration    
      (identifier ":" type)
      val-decl)


    (module-body
      ("[" (arbno definition) "]")
      defns-module-body)


    (definition
      (identifier "=" expression)
      val-defn)


    ;; new expression:

    (expression
      ("from" identifier (arbno "take" identifier))
      qualified-var-exp)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; no changes in grammar below here
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (expression (number) const-exp)

    (expression
      (identifier)
      var-exp)

    (expression
      ("-" "(" expression "," expression ")")
      diff-exp)
    
    (expression
      ("zero?" "(" expression ")")
      zero?-exp)

    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)

    (expression
      ("let" identifier "=" expression "in" expression)
      let-exp)   

    (expression
      ("proc" "(" identifier ":" type ")" expression)
      proc-exp)

    (expression
      ("(" expression expression ")")
      call-exp)

    (expression
      ("letrec"
        type identifier "(" identifier ":" type ")"
        "=" expression "in" expression)
      letrec-exp)

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
      (interface)
      interface-type)
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; syntactic tests and observers ;;;;;;;;;;;;;;;;

;;;; for types
(define interface-type?
  (lambda (ty)
    (cases type ty
      (interface-type (iface)
        #t)
      (else #f))))

(define interface-type->iface
  (lambda (ty)
    (cases type ty
      (interface-type (iface)
        iface)
      (else (eopl:error 'interface-type->iface)))))

(define interface-type->decls
  (lambda (ty)
    (cases type ty
      (interface-type (iface)
        (cases interface iface
          (simple-iface (decls) decls)))
      (else (eopl:error 'interface-type->iface)))))

(define atomic-type?
  (lambda (ty)
    (cases type ty
      (proc-type (ty1 ty2) #f)
      (else #t))))

(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (t1 t2) #t)
      (else #f))))

(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) arg-type)
      (else (eopl:error 'proc-type->arg-type
              "Not a proc type: ~s" ty)))))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) result-type)
      (else (eopl:error 'proc-type->result-types
              "Not a proc type: ~s" ty)))))

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
      (interface-type (iface)
        (cases interface iface
          (simple-iface (decls)
            #f)))
      ))
)


;;;; for module definitions

;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Maybe(Defn)
(define maybe-lookup-module-in-list
  (lambda (name module-defs)
    (if (null? module-defs)
      #f
      (let ((name1 (module-definition->name (car module-defs))))
        (if (eqv? name1 name)
          (car module-defs)
          (maybe-lookup-module-in-list name (cdr module-defs)))))))

;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Defn OR Error
(define lookup-module-in-list
  (lambda (name module-defs)
    (cond
      ((maybe-lookup-module-in-list name module-defs)
        => (lambda (mdef) mdef))
      (else 
        (eopl:error 'lookup-module-in-list
          "unknown module ~s"
          name)))))

(define module-definition->name
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-type sub-defns m-body)
        m-name))))

(define module-definition->interface
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-type sub-defns m-body)
        m-type))))

(define module-definition->body
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-type sub-defns m-body)
        m-body))))

(define module-definition->subdefns
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-type sub-defns m-body)
        sub-defns))))

(define val-decl?
  (lambda (decl)
    (cases declaration decl
      (val-decl (name ty) #t))))

(define decl->name
  (lambda (decl)
    (cases declaration decl
      (val-decl (name ty) name))))

(define decl->type
  (lambda (decl)
    (cases declaration decl
      (val-decl (name ty) ty))))
