#lang racket

(define (all args)
  (if (null? args)
      true
      (and (car args)
           (all (cdr args)))))

(define (divides? a b)
  (= 0 (remainder a b)))

; Thought I'd keep in the spirit of sequences as conventional interfaces.
(define (prime? n)
  (all (map (lambda (x)
              (not (divides? n x)))
            (enumerate-interval 2 (sqrt n)))))
  

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; All unique pairs (i, j) such that 1 ≤ j < i < n
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; compare results to table on pg 122
(prime-sum-pairs 6)
