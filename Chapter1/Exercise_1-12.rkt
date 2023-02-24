#lang eopl

; simp means simplified
; and we can use letrec
(define subst-simp
    (lambda (new old slist)
        (cond 
            ((null? slist) '())
            ((symbol? (car slist)) 
                (if (eqv? (car slist) old) 
                    (cons new (subst-simp new old (cdr slist)))
                    (cons (car slist) (subst-simp new old (cdr slist)))))
            (else (cons
                    (subst-simp new old (car slist))
                    (subst-simp new old (cdr slist))))))
)

; test
(define s-list '((b c) (b () d)))
(display (subst-simp 'a 'b s-list))