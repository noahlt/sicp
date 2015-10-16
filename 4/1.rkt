#lang racket
(require racket/include)

(include "shared.rkt")

; Forcing left-to-right evaluation
;
; I think implementing this imperatively (using tail-recursion) to
; enforce the order of evaluation is the most beautiful way to do this,
; although nobody else on the internet seems to have posted this solution.
; I'm convinced this is correct, because the evaluator must evaluate
; the arguments to the recursive call to make-values only after it has
; evaluated all of its operands, which includes the call to eval on
; first-operand.  So first-operand must be evalled before the rest of
; the values are evalled, which only happens during recursive calls
; to make-values.

(define (sicp-eval x env) x)

(define (make-values exps env values)
  (if (no-operands? exps)
      values
      (make-values (rest-operands exps)
                   env
                   (cons (sicp-eval (first-operand exps) env)
                         values))))

(define (list-of-values-lr exps env)
  (reverse (make-values exps env '())))

(define (list-of-values-rl exps env)
  (make-values (reverse exps) env '()))

; To test:

(define (print-and-return x)
  (display x)
  (display "\n")
  x)

(list-of-values-lr (list (print-and-return 1)
                         (print-and-return 2)
                         (print-and-return 3))
                   '())

(list-of-values-rl (list (print-and-return 1)
                         (print-and-return 2)
                         (print-and-return 3))
                   '())