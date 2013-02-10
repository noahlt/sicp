#lang racket

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n a)
  (= (expmod a n n) a))

(define (display-appears-prime n)
  (display "the fermat test says ")
  (display n)
  (display " is prime.\n"))

(define (display-alert-not-prime n a)
  (display "the fermat test says ")
  (display n)
  (display " is not prime, because when a = ")
  (display a)
  (display ", a^n is not congruent to a modulo n.\n"))

(define (test-fermat-test n)
  (define (test a)
    (cond ((= a n)
           (display-appears-prime n))
          ((fermat-test n a)
           (test (+ 1 a)))
          (else
           (display-alert-not-prime n a))))
  (test 1))

(test-fermat-test 561)
(test-fermat-test 1105)
(test-fermat-test 1729)
(test-fermat-test 2465)
(test-fermat-test 2821)
(test-fermat-test 6601)
