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

;;; exercise 3.65

(define (partial-sums s)
  (add-streams s
               (stream-cons 0
                            (partial-sums s))))

(define (negate-alternating s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (sn (stream-cdr (stream-cdr s))))
    (stream-cons s0 (stream-cons (- s1) (negate-alternating sn)))))

(define (reciprocal s)
  (stream-map (lambda (x) (/ 1 x)) s))

(define ln2
  (stream-map exact->inexact
              (partial-sums (negate-alternating (reciprocal counting-numbers)))))

(define (d s)
  (display-stream (stream-take 1000 s)))

(d ln2)

; Even after 1000 iterations, this partial sum still hasn't quite converged
; on ln(2) = 0.693 -- at iteration 1000 it's still alternating between
; 0.6936 and 0.6926.
