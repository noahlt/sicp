#lang racket

; make-semaphore implemented with mutexes

(define (make-semaphore n)
  (let ((mutex (make-mutex))
        (count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (< count n)
                 (begin (set! count (+ 1 count))
                        (mutex 'release))
                 (begin (mutex 'release)
                        (the-semaphore 'acquire))))
            ((eq? m 'release)
             (mutex 'acquire)
             (begin (set! count (- count 1))
                    (mutex 'release)))))
    the-semaphore))

; make-semephore implemented with test-and-set!

(define (n-times n x)
  (if (= n 0)
      (list)
      (cons x (n-times (- n 1)))))

(define (make-semaphore n)
  (let ((cells (n-times n false)))
    (define (find-and-set-cell! remaining-cells)
      (if (null? remaining-cells)
          true
          (if (test-and-set! remaining-cells)
              (find-and-set-cell! (cdr remaining-cells))
              false)))
    (define (find-and-clear-cell! remaining-cells)
      (if (null? remaining-cells)
          ; should never happen. maybe in practice should be a no-op rather than an error:
          (error "attempted to release semaphore when all cells have already been released")
          ; this seems dicey but should be okay since nobody ever writes to true cells:
          (if (car cell)
              (set-car! cell false)
              (find-and-clear-cell! (cdr remaining-cells)))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (find-and-set-cell! cells)
                 (the-semaphore 'acquire)))
            ((eq? m 'release) (find-and-clear-cell))))
    the-semaphore))