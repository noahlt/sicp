#lang r5rs

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (even? n)
  (= 0 (remainder n 2)))

(define (odd? n)
  (= 1 (remainder n 2)))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        ((odd? count)
         (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)))))


