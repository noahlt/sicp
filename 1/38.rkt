#lang racket

(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i)
                         (+ (d i) res)))))
  (iter k 0))

(define (constant k)
  (lambda (x) k))

(define (divides? a b)
  (= 0 (remainder a b)))

(define (derive-e precision)
  (+ 2 (cont-frac (constant 1.0)
                  (lambda (n)
                    (if (divides? (+ n 1) 3)
                        (* 2 (/ (+ n 1) 3))
                        1))
                  precision)))

(derive-e 20)