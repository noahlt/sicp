#lang racket

(define (square x)
  (* x x))

(define (square-list1 items)
  (if (null? items)
      (list)
      (cons (square (car items)) (square-list1 (cdr items)))))

(square-list1 (list 1 2 3 4 5 6 7))

(define (square-list2 items)
  (map square items))

(square-list2 (list 1 2 3 4 5 6 7))