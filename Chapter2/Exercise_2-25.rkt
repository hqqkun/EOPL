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

; bintree information
(define-datatype bintree-info bintree-info?
  (leaf-info 
    (num integer?))
  (interior-info
    (max-symbol symbol?)
    (max-sub-sum integer?)
    (whole-tree-sum integer?))
)


; Bintree -> Symbol
(define max-interior
  (lambda (tree)
    (let ([tree-info (make-bintree-info tree)])
      (cases bintree-info tree-info
        (leaf-info (_) (eopl:error 'max-interior "tree is leaf.~%"))
        (interior-info (sym _ __) sym))))
)

; Bintree -> Bintree-info
(define make-bintree-info
  (lambda (tree)
    (cases bintree tree
      (leaf-node (num) (leaf-info num))
      (interior-node (key left right)
        (let
          ( [left-info (make-bintree-info left)]
            [right-info (make-bintree-info right)])
          
          (cases bintree-info left-info
            (leaf-info (a) 
              (cases bintree-info right-info
                (leaf-info (b) (interior-info key (+ a b) (+ a b)))
                (interior-info (right-sym right-max right-sum)
                  (let ([whole-sum (+ a right-sum)])
                    (cond
                      ((< a 0) (interior-info right-sym right-max whole-sum))
                      ((< right-max whole-sum) (interior-info key whole-sum whole-sum))
                      (else (interior-info right-sym right-sum whole-sum)))))))
            
            (interior-info (left-sym left-max left-sum)
              (cases bintree-info right-info
                (leaf-info (b)
                  (let ([whole-sum (+ left-sum b)])
                    (cond
                      ((< b 0) (interior-info left-sym left-max whole-sum))
                      ((< left-max whole-sum) (interior-info key whole-sum whole-sum))
                      (else (interior-info left-sym left-max whole-sum)))))
                
                (interior-info (right-sym right-max right-sum)
                  (let ([whole-sum (+ left-sum right-sum)])
                    (cond
                      ((and (> left-max whole-sum) (> left-max right-max)) 
                        (interior-info left-sym left-max whole-sum))
                      ((and (> right-max whole-sum) (> right-max left-max))
                        (interior-info right-sym right-max whole-sum))
                      (else (interior-info key whole-sum whole-sum))))))))))))
)

; test
(define tree-c (interior-node 'C (leaf-node 4) (leaf-node 4)))
(define tree-b (interior-node 'B tree-c (leaf-node -8)))
(define tree-a (interior-node 'A tree-b (leaf-node 1)))
(define leaf (leaf-node 12))

(display (max-interior tree-a))