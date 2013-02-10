#lang racket

(define (runtime)
  (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime n (- (runtime) start-time))
      (display "")))

(define (report-prime n elapsed-time)
  (display " *** ")
  (display elapsed-time)
  true)

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


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

(newline)
(display "checking for primes above 1000000...")
(check-for-primes 1000000 1000050)

; Checking for    Fermat Test   Euclid's Method
; primes around   (avg time)     (avg time)
;      1000          0.015         0.01
;     10000          0.016         0.03
;    100000          0.020         0.09
;   1000000          0.021

; On my machine, the Fermat test actually runs slightly slower than Euclid's
; Method when checking for primes around 1000.  But the Fermat Test slows
; hardly at all when n increases by two orders of magnitude, while Euclid's
; Method slows by almost an order of magnitude.
;
; Given our analysiss of the Fermat Test as a logarithmic algorithm, we
; would expect its running time to increase linearly while the size of its
; input increased exponentially.  The data bear out this expectation.
