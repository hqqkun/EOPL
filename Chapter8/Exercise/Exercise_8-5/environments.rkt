#lang eopl

(require "drscheme-init.rkt")
(require "data-structures.rkt")
(require "lang.rkt")


(provide empty-env extend-env apply-env)
(provide lookup-module-name-in-env)
(provide lookup-qualified-var-in-env)

(define inital-value-env 
  (lambda (m-env)
    (extend-env 
      'i (num-val 1)
      (extend-env
        'v (num-val 5)
        (extend-env
          'x (num-val 10)
          (empty-env m-env)))))
)

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
        (eopl:error 'apply-env "No value binding for ~s" search-sym))
      (extend-env (bvar bval saved-env)
        (if (eqv? search-sym bvar)
          bval
          (apply-env saved-env search-sym)))
      (extend-env-recursively
        (ids bvars bodies saved-env)
        (let ( [loc (location search-sym ids)])
          (if loc
            (proc-val 
              (procedure 
                (list-ref bvars loc)
                (list-ref bodies loc)
                env))
            (apply-env saved-env search-sym))))
      (extend-env-with-module
          (m-name m-val saved-env)
        (apply-env saved-env search-sym))))
)

(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n)
            (+ n 1)))
      (else #f)))
)

;; lookup-module-name-in-env : Sym * Env -> Typed-Module
(define lookup-module-name-in-env
  (lambda (m-name env)
    (cases environment env
      (empty-env ()
          (eopl:error 'lookup-module-name-in-env
            "No module binding for ~s" m-name))
      (extend-env (bvar bval saved-env)
        (lookup-module-name-in-env m-name saved-env))
      (extend-env-recursively
        (ids bvars bodies saved-env)
        (lookup-module-name-in-env m-name saved-env))
      (extend-env-with-module
          (m-name1 m-val saved-env)
        (if (eqv? m-name m-name1)
          m-val
          (lookup-module-name-in-env m-name saved-env)))
    ))
)

 ;; lookup-qualified-var-in-env : Sym * Sym * Env -> ExpVal
(define lookup-qualified-var-in-env
  (lambda (m-name var-name env)
    (let ([m-val (lookup-module-name-in-env m-name env)])
      (cases typed-module m-val
        (simple-module (bindings)
          (apply-env bindings var-name)))
    ))
)
