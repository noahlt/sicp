#lang racket

;;; general utilities

(define (incr n) (+ n 1))
(define (decr n) (- n 1))

;;; delay & force

(define (memo-proc proc)
  (let ((has-run false)
        (result false))
    (lambda ()
      (if has-run
          result
          (begin (set! result (proc))
                 (set! has-run true)
                 result)))))

(define-syntax-rule (delay exp)
  (memo-proc (lambda () exp)))

(define-syntax-rule (force delayed-object)
  (delayed-object))

;;; stream primitives

(define the-empty-stream null)
(define stream-null? null?)

(define-syntax-rule (stream-cons a b)
  (cons a (delay b)))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

;;; stream utilities

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (decr n))))

(define (stream-map f . streams)
  (if (ormap stream-null? streams)
      the-empty-stream
      (stream-cons (apply f (map stream-car streams))
                   (apply stream-map f (map stream-cdr streams)))))

(define (stream-for-each f s)
  (if (stream-null? s)
      'done
      (begin (f (stream-car s))
             (stream-for-each f (stream-cdr s)))))

(define (stream-filter pred? s)
  (cond ((stream-null? s)
         the-empty-stream)
        ((pred? (stream-car s))
         (stream-cons (stream-car s)
                      (stream-filter pred? (stream-cdr s))))
        (else
         (stream-filter pred? (stream-cdr s)))))

(define (stream-take n s)
  (if (= n 0)
      the-empty-stream
      (stream-cons (stream-car s)
                   (stream-take (decr n) (stream-cdr s)))))

(define (add-streams . streams)
  (apply stream-map + streams))

(define (mul-streams . streams)
  (apply stream-map * streams))

(define (scale-stream x s)
  (stream-cons (* x (stream-car s))
               (scale-stream x (stream-cdr s))))

(define (div-streams . streams)
  (apply stream-map / streams))

(define (negate-stream s)
  (stream-map - s))

;;; stream display

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

;;; common streams

(define (stream-enumerate-interval start end)
  (if (> start end)
      the-empty-stream
      (stream-cons start
                   (stream-enumerate-interval (incr start) end))))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 0))

(define counting-numbers (integers-starting-from 1))

(define zeroes (stream-cons 0 zeroes))

(define ones (stream-cons 1 ones))

;;; (from exercise 59)

(define (integrate-series s)
  (div-streams s counting-numbers))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cos-series
  (stream-cons 1 (negate-stream (integrate-series sin-series))))

(define sin-series
  (stream-cons 0 (integrate-series cos-series)))

;;; (from exercise 60)

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-car s1)
                                          (stream-cdr s2))
                            (mul-series (stream-cdr s1)
                                        s2))))
;;; (from exercise 61)

(define (invert-unit-series S)
  (stream-cons 1
               (negate-stream (mul-series (stream-cdr S)
                                          (invert-unit-series S)))))

;;; exercise 62

(define (div-series num-series denom-series)
  (let ((c (stream-car denom-series)))
    (if (= c 0)
        (error "denominator has a zero constant term")
        (mul-series num-series
                    (scale-stream c (invert-unit-series denom-series))))))

(define tan-series (div-series sin-series cos-series))

(define (d s)
  (display-stream (stream-take 10 s)))

(d tan-series)