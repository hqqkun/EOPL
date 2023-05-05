#lang eopl

(require "drscheme-init.rkt")
(require "cps-in-lang.rkt")
(require "cps-out-lang.rkt")

(provide cps-of-program)


(define cps-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (cps-a-program
          (cps-of-exps (list exp1)
            (lambda (new-rands)
              (simple-exp->exp (car new-rands))))))))
)


; cps-of-exp : Exp * SimpleExp -> TfExp
(define cps-of-exp
  (lambda (exp cont)
    (cases expression exp
      (const-exp (num)
        (make-send-to-cont cont (cps-const-exp num)))
      (var-exp (var)
        (make-send-to-cont cont (cps-var-exp var)))
      (proc-exp (vars body)
        (make-send-to-cont cont
          (cps-proc-exp
            (append vars (list 'k%00))
            (cps-of-exp body (cps-var-exp 'k%00)))))
      (zero?-exp (exp1)
        (cps-of-zero?-exp exp1 cont))
      (diff-exp (exp1 exp2)
        (cps-of-diff-exp exp1 exp2 cont))
      (if-exp (exp1 exp2 exp3)
        (cps-of-if-exp exp1 exp2 exp3 cont))
      (let-exp (var exp1 body)
        (cps-of-let-exp var exp1 body cont))
      (letrec-exp (p-names bvarss proc-bodies body)
        (cps-of-letrec-exp p-names bvarss proc-bodies body cont))
      (call-exp (rator rands)
        (cps-of-call-exp rator rands cont))
      (sum-exp (exps)
        (cps-of-sum-exp exps cont))
      ))
)



;; cps-of-exps : Listof(InpExp) * (Listof(InpExp) -> TfExp) 
;;                -> TfExp
(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ([exps exps])
      (let
        ([pos (list-index
                (lambda (exp) (not (inp-exp-simple? exp))) exps)])
        (if (not pos)
          (builder (map cps-of-simple-exp exps))
          (let ([new-var (fresh-identifier 'var)])
            (cps-of-exp (list-ref exps pos)
              (cps-proc-exp 
                (list new-var)
                (cps-of-rest 
                  (list-set exps pos (var-exp new-var))))))))))
)

(define cps-of-call-exp
  (lambda (rator rands k-exp)
    (cps-of-exps (cons rator rands)
      (lambda (new-rands)
        (cps-call-exp 
          (car new-rands)
          (append (cdr new-rands) (list k-exp))))))
)

(define cps-of-sum-exp
  (lambda (exps k-exp)
    (cps-of-exps exps
      (lambda (new-rands)
        (make-send-to-cont k-exp (cps-sum-exp new-rands)))))
)

(define cps-of-letrec-exp
  (lambda (p-names bvarss proc-bodies body cont)
    (cps-letrec-exp
      p-names
      (map (lambda (bvars) (append bvars (list 'k%00))) bvarss)
      (map (lambda (exp) (cps-of-exp exp (cps-var-exp 'k%00))) proc-bodies)
      (cps-of-exp body cont)))
)

(define cps-of-let-exp
  (lambda (var exp1 body k-exp)
    ; with proc call
    (cps-of-exp
      (call-exp
        (proc-exp 
          (list var)
          body)
        (list exp1)) k-exp))
)

(define cps-of-zero?-exp
  (lambda (exp k-exp)
    (cps-of-exps (list exp)
      (lambda (new-rands)
        (make-send-to-cont k-exp
          (cps-zero?-exp (car new-rands))))))
)

(define cps-of-diff-exp
  (lambda (exp1 exp2 k-exp)
    (cps-of-exps (list exp1 exp2)
      (lambda (new-rands)
        (make-send-to-cont k-exp
          (cps-diff-exp (car new-rands) (cadr new-rands))))))
)
      
(define cps-of-if-exp
  (lambda (exp1 exp2 exp3 k-exp)
    (cps-of-exps (list exp1)
      (lambda (new-rands)
        (cps-if-exp 
          (car new-rands)
          (cps-of-exp exp2 k-exp)
          (cps-of-exp exp3 k-exp)))))
)


(define make-send-to-cont
  (lambda (k-exp simple-exp)
    (cases simple-expression k-exp
      (cps-proc-exp (vars body)
        (replace-tail-exp-with-free-var body simple-exp (car vars)))
      (else (cps-call-exp k-exp (list simple-exp))))
    )
)

(define replace-tail-exp-with-free-var
  (lambda (exp r-exp f-var)
    (cases tfexp exp
      (simple-exp->exp (exp1)
        (simple-exp->exp (replace-simple-exp-with-free-var exp1 r-exp f-var)))
      (cps-if-exp (exp1 exp2 exp3)
        (cps-if-exp 
          (replace-simple-exp-with-free-var exp1 r-exp f-var)
          (replace-tail-exp-with-free-var exp2 r-exp f-var)
          (replace-tail-exp-with-free-var exp3 r-exp f-var)))
      (cps-call-exp (rator rands)
        (cps-call-exp
          (replace-simple-exp-with-free-var rator r-exp f-var)
          (map (lambda (exp) (replace-simple-exp-with-free-var exp r-exp f-var)) rands)))
      (else exp)
    ))
)

(define replace-simple-exp-with-free-var
  (lambda (exp r-exp f-var)
    (cases simple-expression exp
      (cps-diff-exp (exp1 exp2)
        (cps-diff-exp
          (replace-simple-exp-with-free-var exp1 r-exp f-var)
          (replace-simple-exp-with-free-var exp2 r-exp f-var)))
      (cps-const-exp (num)
        exp)
      (cps-zero?-exp (exp1)
        (cps-zero?-exp (replace-simple-exp-with-free-var exp1 r-exp f-var)))
      (cps-sum-exp (exps)
        (cps-sum-exp
          (map (lambda (exp) (replace-simple-exp-with-free-var exp r-exp f-var)) exps)))
      (cps-var-exp (var)
        (if (eqv? var f-var)
          r-exp
          exp))
      (cps-proc-exp (vars body)
        (if (list-index (lambda (val) (eqv? val f-var)) vars)
          exp
          (cps-proc-exp vars
            (replace-tail-exp-with-free-var body r-exp f-var))))
    ))
)


(define cps-of-simple-exp
  (lambda (exp)
    (cases expression exp
      (const-exp (num)
        (cps-const-exp num))
      (var-exp (var)
        (cps-var-exp var))
      (diff-exp (exp1 exp2)
        (cps-diff-exp
          (cps-of-simple-exp exp1)
          (cps-of-simple-exp exp2)))
      (zero?-exp (exp1)
        (cps-zero?-exp
          (cps-of-simple-exp exp1)))
      (proc-exp (vars body)
        (cps-proc-exp
          (append vars (list 'k%00))
          (cps-of-exp body (cps-var-exp 'k%00))))
      (sum-exp (exps)
        (cps-sum-exp
          (map cps-of-simple-exp exps)))
      (else (report-invalid-exp-to-cps-of-simple-exp exp))

    ))
)

(define report-invalid-exp-to-cps-of-simple-exp
  (lambda (exp)
    (eopl:error 'cps-simple-of-exp
      "non-simple expression to cps-of-simple-exp: ~s"
      exp))
)

;;;;;;;;;;;;;;;; utilities ;;;;;;;;;;;;;;;;
(define list-index
  (lambda (pred lst)
    (letrec
        ([L (lambda (lst index)
              (cond
                ((null? lst) #f)
                ((pred (car lst)) index)
                (else (L (cdr lst) (+ index 1)))))])
      (L lst 0)))
)


(define list-set
  (lambda (lst n val)
    (cond
      ((null? lst) (eopl:error 'list-set "ran off end"))
      ((zero? n) (cons val (cdr lst)))
      (else (cons (car lst) (list-set (cdr lst) (- n 1) val)))))
)

(define fresh-identifier
  (let ([sn 0])
    (lambda (identifier)
      (set! sn (+ 1 sn))
      (string->symbol
        (string-append 
          (symbol->string identifier)
          "%"
          (number->string sn)))))
)

(define inp-exp-simple?
  (lambda (exp)
    (cases expression exp
      (const-exp (num) #t)
      (var-exp (var) #t)
      (proc-exp (vars body) #t)
      (diff-exp (exp1 exp2)
        (and (inp-exp-simple? exp1)
          (inp-exp-simple? exp2)))
      (zero?-exp (exp1)
        (inp-exp-simple? exp1))
      (sum-exp (exps)
        (all-simple? exps))
      (else #f)
    ))
)

(define all-simple?
  (lambda (exps)
    (if (null? exps)
       #t
       (and 
        (inp-exp-simple? (car exps))
        (all-simple? (cdr exps)))))
)
