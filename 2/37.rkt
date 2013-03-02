#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define example-matrix
  '((1 2 3)
    (4 5 6)
    (7 8 9)))

(define example-vector
  '(1 2 3))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col)
                  (dot-product m-row n-col))
                n-cols))
         m)))

; should be (14 32 50)
(matrix-*-vector example-matrix example-vector)

; should be ((1 4 7) (2 5 8) (3 6 9))
(transpose example-matrix)

; should be ((30 36 42) (66 81 96) (102 126 150))
(matrix-*-matrix example-matrix example-matrix)

