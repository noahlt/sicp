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

(define (div-streams . streams)
  (apply stream-map / streams))

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




;;; exercise 3.59a

(define (integrate-series s)
  (div-streams s counting-numbers))

;;; exercise 3.59b

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cos-series
  (stream-cons 1 (stream-map - (integrate-series sin-series))))

(define sin-series
  (stream-cons 0 (integrate-series cos-series)))

;;; checking

(define (check-59)
  (display-line 'exp-series)
  (display-stream (stream-take 5 exp-series))
  (display-line 'cos-series)
  (display-stream (stream-take 5 cos-series))
  (display-line 'sin-series)
  (display-stream (stream-take 6 sin-series)))

(check-59)

