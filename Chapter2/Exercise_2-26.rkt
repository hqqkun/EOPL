#lang eopl

(define-datatype red-blue-tree red-blue-tree?
  (a-red-bule-tree (subtree red-blue-subtree?))
)

(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
   (left red-blue-subtree?)
   (right red-blue-subtree?))
  (blue-node
   (red-blue-subtrees (list-of red-blue-subtree?))
   )
  (leaf-node
   (num number?))
)

; Red-blue-subtree -> Red-blue-subtree
(define mark-helper
  (lambda (subtree red-depth)
    (cases red-blue-subtree subtree
      (red-node (left right)
                (red-node
                 (mark-helper left  (+ 1 red-depth))
                 (mark-helper right (+ 1 red-depth))))
      (leaf-node (_) (leaf-node red-depth))
      (blue-node (subtrees)
                 (blue-node
                  (map (lambda (tree) (mark-helper tree red-depth)) subtrees)))))
  )


(define mark-leaves-with-red-depth
  (lambda (tree)
    (cases red-blue-tree tree
      (a-red-bule-tree (subtree) (a-red-bule-tree (mark-helper subtree 0)))
      ))
)

; test
(define t1 (leaf-node 12))
(define t2 (blue-node (list t1 t1 t1)))
(define t3 (red-node t2 t2))
(define rd-tree (a-red-bule-tree t3))
(display rd-tree)
(newline)
(display (mark-leaves-with-red-depth rd-tree))