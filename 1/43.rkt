#lang racket

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))


(define (square x)
  (* x x))

((repeated square 2) 5)