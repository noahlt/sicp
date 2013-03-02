#lang racket

(define (sum seq)
  (foldr + 0 seq))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(define (make-list-sum seq)
  (append seq (list (sum seq))))

; All unique pairs (i, j) such that 1 ≤ j < i < n
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; All unique triples (i, j, k) such that 1 ≤ k < j < i < n
(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (pair) (cons i pair))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))
             
; All unique triples (a, b, c) such that 1 ≤ a < b < c < n
; and a + b + c = k
(define (k-sum-triples n k)
  (map make-list-sum
       (filter (lambda (triple)
                 (= k (sum triple)))
               (unique-triples n))))