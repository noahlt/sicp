#lang racket

; '(3 7 2 8 5 1 4 6)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (incr x) (+ x 1))
(define (decr x) (- x 1))

(define (safe? k positions)
  (define (check direction next-position remaining-board)
    (or (null? remaining-board)
        (and (not (= next-position (car remaining-board)))
             (check direction
                    (direction next-position)
                    (cdr remaining-board)))))
  (and (check incr (incr (car positions)) (cdr positions))
       (check identity (identity (car positions)) (cdr positions))
       (check decr (decr (car positions)) (cdr positions))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          ; rest-of-queens is a way to place k-1 queens in the first k-1 cols
          (lambda (rest-of-queens)
            ; new-row is a proposed row in which to place the queen in the kth col
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
