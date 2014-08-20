#lang racket

(define f ; note we are not using the syntax sugared function definition!
  (let ((state 0))
    (lambda (new-state)
      (let ((old-state state))
        (set! state new-state)
        old-state))))