#lang racket

((compose car cdr car cdr cdr)
 '(1 3 (5 7) 9))

((compose car car)
 '((7)))

((compose car cdr car cdr car cdr car cdr car cdr car cdr)
 '(1 (2 (3 (4 (5 (6 7)))))))