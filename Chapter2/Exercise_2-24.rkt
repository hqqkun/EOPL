#lang eopl

; bintree
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?))
)

; bintree -> List
(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) (list 'leaf-node num))
      (interior-node (key left right)
        (list 
          'interior-node
          (bintree-to-list left)
          (bintree-to-list right)))))
)

; test
(define tree  (interior-node 'a (leaf-node 3) (leaf-node 4)))
(display tree)
(newline)
(display (bintree-to-list tree))