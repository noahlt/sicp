#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (match-sign? interval lower-sign upper-sign)
  (and (or (and (equal? lower-sign -) (< (lower-bound interval) 0))
           (and (equal? lower-sign +) (>= (lower-bound interval) 0)))
       (or (and (equal? upper-sign -) (< (upper-bound interval) 0))
           (and (equal? upper-sign +) (>= (upper-bound interval) 0)))))

(define (mul-interval x y)
  ; Signs takes four signs and compares them to the signs of x and y. Returns
  ; true if they match.
  (define (signs x-lower x-upper y-lower y-upper)
    (and (match-sign? x x-lower x-upper)
         (match-sign? y y-lower y-upper)))
  (define (ll) (* (lower-bound x) (lower-bound y)))
  (define (lu) (* (lower-bound x) (upper-bound y)))
  (define (ul) (* (upper-bound x) (lower-bound y)))
  (define (uu) (* (upper-bound x) (upper-bound y)))
  ; I really like this solution, because it's super-easy to debug. You can
  ; check each case on a REPL and if it's wrong, it's easy to make the
  ; correction in code. This is the first time I've done anything I would
  ; call domain-driven design.
  (cond ((signs + + + +) (make-interval (ll) (uu)))
        ((signs - + + +) (make-interval (lu) (uu)))
        ((signs - - + +) (make-interval (lu) (ul)))
        ((signs - - - +) (make-interval (lu) (ll)))
        ((signs - - - -) (make-interval (uu) (ll)))
        ((signs - + - -) (make-interval (ul) (ll)))
        ((signs + + - -) (make-interval (ul) (lu)))
        ((signs + + - +) (make-interval (ul) (uu)))
        ((signs - + - +) (let ((ll (ll)) (lu (lu)) (ul (ul)) (uu (uu)))
                           (make-interval (min ll lu ul uu)
                                          (max ll lu ul uu))))))
