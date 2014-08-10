#lang racket

(define (make-monitored f)
  (let ((call-count 0))
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          call-count
          (begin
            (set! call-count (+ 1 call-count))
            (f x))))))



(define s (make-monitored sqrt))

(s 100) ; 10

(s 'how-many-calls?) ; 1

(s 16) ; 4

(s 'how-many-calls?) ; 2