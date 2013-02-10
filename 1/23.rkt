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
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

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


; On my machine, checking primes around 1000 took about 0.01ms, checking
; primes around 10000 took around 0.02ms, and checking primes around 100000
; took around 0.05ms.
;
; This modified algorithm does, in fact, run about twice as fast as the
; original (see 22.rkt for original runnig times), but only when we get to
; numbers around 100000.  Around 1000 and 10000, this modified algorithm runs
; about as fast as our original algorithm. I don't know why, but this
; illustrates the value of data-driven vs purely theoretical analysis.
