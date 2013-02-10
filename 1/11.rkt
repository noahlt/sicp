#lang r5rs

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (f-helper a b c count)
    (if (= count n)
        c
        ; a = f(count - 3)
        ; b = f(count - 2)
        ; c = f(count - 1)
        (f-helper b c (+ c (* 2 b) (* 3 a)) (+ count 1))))
  (if (< n 3)
      n
      (f-helper 0 1 2 2)))

;; testing code follows

(define (fencepost post vals)
  (if (= 1 (length vals))
      vals
      (cons (car vals) (cons post (fencepost post (cdr vals))))))

(define (display-tabular . vals)
  (display (apply string-append (fencepost "\t" (map number->string vals))))
  (display "\n"))

(define (display-table number-of-rows)
  (define (iterator count)
    (display-tabular count (f-rec count) (f-iter count))
    (if (<= count number-of-rows)
        (iterator (+ count 1))))
  (display "n\tf-rec\tf-iter\n")
  (iterator 0))
  
(display-table 5)