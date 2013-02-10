#lang racket

(define (accumulate-recurse combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recurse combiner null-value term (next a) next b))))

(define (product-recurse term a next b)
  (accumulate-recurse * 1 term a next b))

(define (sum-recurse term a next b)
  (accumulate-recurse + 0 term a next b))



(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))


; The presence of the > operator in accumulate really bothers me.
