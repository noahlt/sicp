#lang racket

(define (square x)
  (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))

(define (make-circle-p cx cy r)
  (lambda (tx ty)
    (<= (+ (square (- tx cx))
           (square (- ty cy)))
        (square r))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 y1 x2 y2)
  ; One million is the number of tests I needed to consistenly get
  ; a number that started with 3.14...
  (* (monte-carlo 1000000 (lambda ()
                           (let ((x (random-in-range x1 x2))
                                 (y (random-in-range y1 y2)))
                             (p x y))))
     (* (- x2 x1) (- y2 y1))))

(* 1.0 (estimate-integral (make-circle-p 0 0 1) -1 -1 1 1))