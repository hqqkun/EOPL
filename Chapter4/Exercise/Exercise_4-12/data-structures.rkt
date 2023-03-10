#lang eopl

(require "lang.rkt")                  ; for expression?

(provide (all-defined-out))           ; too many things to list

;;;;;;;;;;;;;;;;;;;; answer ;;;;;;;;;;;;;;;;;;;;;;
(define-datatype answer answer?
  (an-answer
    (val expval?)
    (store store?))
)

; extractors
(define answer->val
  (lambda (answer1)
    (cases answer answer1
     (an-answer (val store) val)))    
)

(define answer->store
  (lambda (answer1)
    (cases answer answer1
      (an-answer (val store) store))))

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

(define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?)))

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
    (bvar symbol?)
    (body expression?)
    (env environment?))
)
  
(define-datatype environment environment?
  (empty-env)
  (extend-env 
    (bvar symbol?)
    (bval expval?)
    (saved-env environment?))
  (extend-env-rec*
    (proc-names (list-of symbol?))
    (b-vars (list-of symbol?))
    (proc-bodies (list-of expression?))
    (saved-env environment?))
)

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))


;;;;;;;;;;;;;;;;;;;; store ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define store? list?)
(define reference? integer?)

(define empty-store
  (lambda () '())
)

; newref : store * ExpVal -> Answer
(define newref
  (lambda (store val)
    (let
      ( [next-ref (length store)]
        [new-store (append store (list val))])
    
      (an-answer
          (ref-val next-ref)
          new-store)))
)

; deref : Store * Ref -> ExpVal
(define deref
  (lambda (store ref)
    (list-ref store ref))
)

;; setref! : Store * Ref * ExpVal -> Store
(define setref!
  (lambda (store ref val)
    (letrec
      ( [S  (lambda (ref1 store1)
              (cond
                ((null? store1) (report-invalid-reference ref store))
                ((zero? ref1) (cons val) (cdr store1))
                (else (cons (car store1) (S (- ref1 1) (cdr store1))))))])
      (S ref store)))
)

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store))
)
