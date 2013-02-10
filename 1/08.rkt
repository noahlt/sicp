(define (cuberoot-iter guess x)
  (display guess)
  (display "\n")
  (if (good-enough? guess x)
      guess
      (cuberoot-iter (improve guess x)
                     x)))

(define (cuberoot x)
  (cuberoot-iter 1 (exact->inexact x)))

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (difference a b)
  (abs (- a b)))

(define (good-enough? guess x)
  (< (difference (cube guess) x) 0.001))

(define (improve y x)
  (/ (+ (/ x (square y))
        (* 2 y))
     3))


