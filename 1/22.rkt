#lang racket

(define (runtime)
  (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      (display "")))

(define (report-prime n elapsed-time)
  (display " *** ")
  (display elapsed-time)
  true)




(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))



(define (check-for-primes a b)
  (if (< a b)
      (if (odd? a)
          (and (timed-prime-test a)
               (check-for-primes (+ 1 a) b))
          (check-for-primes (+ 1 a) b))
      (newline)))

(display "checking for primes above 1000...")
(check-for-primes 1000 1050)

(newline)
(display "checking for primes above 10000...")
(check-for-primes 10000 10050)

(newline)
(display "checking for primes above 100000...")
(check-for-primes 100000 100050)

;  ____           _____           ______
; √1000 ~= 31    √10000 = 100    √100000 ~= 316
;      _____                      ____       ______               _____
; So, √10000 is approximately 3x √1000, and √100000 is approx 3x √10000
;
; On my machine, checking for primes around 1000 takes about 0.01ms, while
; checking for primes around 10000 takes about 0.028ms, and checking for
; primes around 100000 takes about 0.09ms.
;
; So, on my machine the time to check primes around 10000 took about 3x the
; time to check primes around 1000, and likewise for 100000 and 10000.  This
; data is in line with our O(√n) prediction, and ultimately supports the
; notion that programs run in time proportional to the number of steps
; required for computation.

