#lang racket

(define (cont-frac-recurse n d k)
  (define (helper i)
    (if (= i k)
        0
        (/ (n i)
           (+ (d i) (helper (+ 1 i))))))
  (helper 1))

(define (cont-frac-iter n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i)
                         (+ (d i) res)))))
  (iter k 0))

(define (constant k)
  (lambda (x) k))

(define (derive-phi-recurse precision)
  (/ 1 (cont-frac-recurse (constant 1.0) (constant 1.0) precision)))

(define (derive-phi-iter precision)
  (/ 1 (cont-frac-iter (constant 1.0) (constant 1.0) precision)))

; Takes 14 steps to get four digits of precision. Interestingly enough,
; the iterative and recursive forms do not compute the same values at the
; same number of steps:
;
;     > (derive-phi-recurse 14)
;     1.6180257510729614
;     > (derive-phi-iter 14)
;     1.6180371352785146
;     > 
;
; The iterative version is noticeably faster.