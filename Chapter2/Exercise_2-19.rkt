#lang eopl

; Bintree ::= () | (Int Bintree Bintree)

(define number->bintree
  (lambda (num) (list num '() '()))
)

(define current-element car)
(define at-leaf? null?)
(define move-to-left-son cadr)
(define move-to-right-son caddr)

(define insert-to-left
  (lambda (new-num bintree)
    (let (
          (num (current-element bintree))
          (left (move-to-left-son bintree))
          (right (move-to-right-son bintree)))
      (list
       num
       (list new-num left '())
       right)))
)

(define insert-to-right
  (lambda (new-num bintree)
    (let (
          (num (current-element bintree))
          (left (move-to-left-son bintree))
          (right (move-to-right-son bintree)))
      (list
       num
       left
       (list new-num '() right))))
)

; test
(define t1 
  (insert-to-right 14
    (insert-to-left 12
      (number->bintree 13))))