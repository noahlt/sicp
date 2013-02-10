#lang racket

(define (product-recurse term a next b)
  (if (> a b)
      1
      (* (term a) (product-recurse term (next a) next b))))

(define (incr n)
  (+ n 1))


(define (pi-term n)
  (if (odd? n)
      (/ (+ n 1) (+ n 2))
      (/ (+ n 2) (+ n 1))))

(define (pi-recurse n)
  (exact->inexact (* 4 (product-recurse pi-term 1 incr n))))



(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (pi-iter n)
  (exact->inexact (* 4 (product-iter pi-term 1 incr n))))

; On my machine, pi-iter is noticibly faster for n > 1000