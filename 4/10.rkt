#lang racket

(define (binary-op? exp tag)
  (and (pair? exp)
       (= (length exp) 3)
       (eq? tag (cadr exp))))


; Instead of assignment having the syntax (set! x 3), we give
; assignment the syntax (x = 3)
(define (assignment? exp)
  (binary-op? exp '=))

; Okay so now we do the same inlining thing for function application.
;
; It's really dumb.

(define (application? exp) (pair? exp))

(define (every-other l)
  (if (pair? l)
      (cons (car l)
            (if (pair? (cdr l))
                (every-other (cddr l))
                (list)))
      (list)))

(define (elts-equal? lst)
 (andmap (lambda (x) 
        (equal? x (car lst)))
      lst))

; So, if every other list element is the same, we assume that's
; the operator.
; 
; So the expression '(1 + 2 + 3 + 4) would get treated as an
; inline operation where + is the operator and the operands are
; '(1 2 3 4).  If we fail to find a consistent symbol at every
; other position inline, then we fall back to lispy prefix notation.
(define (inline-operator? exp)
  (and (odd? (length exp))
       (elts-equal? (every-other (cdr exp)))))

(define (operator exp)
  (if (inline-operator? exp)
      (every-other (cdr exp))
      (car exp)))

(define (operands exp)
  (if (inline-operator? exp)
      (every-other exp)
      (cdr exp)))

(define (first-operand exp)
  (car (operands exp)))

(define (rest-operands ops)
  (cdr (operands exp)))


      