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

(define (square x)
  (* x x))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (i)
               (+ 1 (* 2 (- i 1))))
             k))

(tan-cf 0.6 15)