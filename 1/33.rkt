#lang racket

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner null-value term
                                     (next a) next b filter))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (square x)
  (* x x))

(define (incr n)
  (+ n 1))

(define (prime? n)
  ; note: returns true for 1, unspecified for n < 1
  (define (euclid possible-factor)
    (cond ((> (square possible-factor) n)
           true)
          ((divides? n possible-factor)
           false)
          (else
           (euclid (incr possible-factor)))))
  (euclid 2))

(define (sum-squares-prime a b)
  (filtered-accumulate + 0 square a incr b prime?))




(define (relatively-prime a b)
  (= (gcd a b) 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-relative-primes n)
  (def (ridiculous-filter x)
    (relative-prime x n))
  (filtered-accumulate * 1 identity 1 incr n ridiculous-filter))
