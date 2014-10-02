#lang racket

(define progress
  ; each row takes the form (chapter completed total)
  '((1 46 46)
    (2 72 97)
    (3 23 82)
    (4 0 79)
    (5 0 52)))

(define (display-repeated n s)
  (if (= n 0)
      (display "")
      (begin
        (display s)
        (display-repeated (- n 1) s))))
  
(for-each (lambda (chapter)
            (display (car chapter))
            (display ":")
            (display-repeated (cadr chapter) "#")
            (display-repeated (- (caddr chapter) (cadr chapter)) ".")
            (newline))
          progress)
