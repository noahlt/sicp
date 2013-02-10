#lang r5rs

(define (pascal row col)
  (cond ((and (= row 0)
              (= col 0))        1)
        
        ((or (< row 0)
             (and (= row 0)
                  (> col 0)))   0)
        
        (else                   (+ (pascal (- row 1) col)
                                   (pascal (- row 1) (- col 1))))))

;; testing code follows

(define (fencepost post vals)
  (if (= 1 (length vals))
      vals
      (cons (car vals) (cons post (fencepost post (cdr vals))))))

(define (display-tabular vals)
  (display (apply string-append (fencepost "\t" (map number->string vals))))
  (display "\n"))

(define (range start end)
  (if (< start end)
      (cons start (range (+ 1 start) end))
      '()))

(define (display-row n)
  (display-tabular (map (lambda (col) (pascal n col)) (range 0 (+ 1 n)))))

(define (display-triangle n)
  (define (iter current-row)
    (display-row current-row)
    (if (< current-row n)
        (iter (+ 1 current-row))))
  (iter 0))
  