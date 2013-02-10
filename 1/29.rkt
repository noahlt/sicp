#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (incr n)
  (+ n 1))

(define (simpson f a b n)
  (define (y k)
    (f (+ a (* k (/ (- b a) n)))))
  (define (simpson-term k)
    (if (even? k)
        (* 2 (y k))
        (* 4 (y k))))
  (* (/ (/ (- b a) n) 3)
     (+ (y 0)
        (sum simpson-term 1 incr (- n 1))
        (y n))))
             