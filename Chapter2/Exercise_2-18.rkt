#lang eopl

; NodeInSequence ::= (Int Listof(Int) Listof(Int))

(define number->sequence
  (lambda (n)
    (list n '() '()))
)

(define current-element car)
(define left-seq cadr)
(define right-seq caddr)

(define insert-to-left
  (lambda (new-num seq)
    (let (
          (num (current-element seq))
          (left (left-seq seq))
          (right (right-seq seq)))
      (list num (cons new-num left) right)))
)

(define insert-to-right
  (lambda (new-num seq)
    (list
     (current-element seq)
     (left-seq seq)
     (cons new-num (right-seq seq))))
)

(define at-left-end?
  (lambda (seq) (null? (left-seq seq)))
)

(define at-right-end?
  (lambda (seq) (null? (right-seq seq)))
)

(define move-to-left
  (lambda (seq)
    (if (at-left-end? seq)
        (report-error 'move-to-left "left is empty.~%")
        (let (
              (num (current-element seq))
              (left (left-seq seq))
              (right (right-seq seq)))
          (list (car left) (cdr left) (cons num right)))))
)

(define move-to-right
  (lambda (seq)
    (if (at-right-end? seq)
        (report-error 'move-to-right "right is empty.~%")
        (let (
              (num (current-element seq))
              (left (left-seq seq))
              (right (right-seq seq)))
          (list (car right) (cons num left) (cdr right))))
    )
)

(define report-error
  (lambda (which-func msg)
    (eopl:error which-func msg))
)

; test
(define seq '(6 (5 4 3 2 1) ()))
(move-to-right seq)