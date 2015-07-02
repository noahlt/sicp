#lang racket

(define factorials
  (stream-cons 1 (mul-streams factorials
                              (integers-starting-from 2))))
