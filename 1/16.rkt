#lang r5rs

(define (fast-expt base power)
  (fast-expt-helper 1 base power))

(define (odd? n)
  (= 1 (remainder n 2)))

(define (square x)
  (* x x))

(define (fast-expt-helper a b n)
  (cond ((= n 0)
         a)
        ((odd? n)
         (fast-expt-helper (* a b) b (- n 1)))
        (else
         (fast-expt-helper a (square b) (/ n 2)))))

