#lang racket

(define (square x)
  (* x x))

;(define (nontriv-sqrt? x n)
;  (and (not (= x 1))
;       (not (= x (- n 1)))
;       true));
;
;(define (square-and-check x n)
;  (if (and (not (= x 1))
;           (not (= x (- n 1)))
;           (= 1 (remainder (square x) n)))
;      0
;      (remainder (square x) n)))

;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (square-and-check (expmod base (/ exp 2) m) m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m)) m))))

;(define (miller-rabin-test n)
;  (define (try-it a)
;    (display "\na = ")
;    (display a)
;    (= (expmod a (- n 1) n) 1))
;  (try-it (+ 1 (random (- n 2)))))

(define (non-trivial-sqrt? x n)
  (and (not (= x 1))
       (not (= x (- n 1)))
       (= (remainder (square x) n) 1)))

(define (zero-if-non-trivial-sqrt x n)
  (if (non-trivial-sqrt? x n) 0 x))

(define (mrt-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder
                       (square
                         (zero-if-non-trivial-sqrt
                           (mrt-expmod base (/ exp 2) m) m)) m))
        (else (remainder (* base (mrt-expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (mrt-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))


(display (miller-rabin-test 561))
(display (miller-rabin-test 1105))
(display (miller-rabin-test 1729)) ;563
(display (miller-rabin-test 2465))

(newline)

(define (miller-rabin-test-all n)
  (define (try-it a)
    (cond ((= a n) #t)
          ((= (mrt-expmod a (- n 1) n) 1) (try-it (+ a 1)))
          (else #f)))
  (try-it 1))

(miller-rabin-test-all 1729)