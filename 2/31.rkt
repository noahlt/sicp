#lang racket

(define (tree-map f tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

(define (square x)
  (* x x))

(define (square-tree tree)
  (tree-map square tree))

(define test-tree (list 1  (list 2 (list 3 4) 5) (list 6 7)))
(square-tree test-tree)