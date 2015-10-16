#lang racket
(require racket/include)
;(include "shared.rkt")

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-let definitions body)
  (cons 'let (cons definitions body)))

(define (let->combination exp)
  (if (symbol? (cadr exp))
      (let* ((name (cadr exp))
             (params (caddr exp))
             (param-names (map car params))
             (param-values (map cadr params))
             (f (make-lambda param-names (list (cadddr exp)))))
        (list 'let (list (list (cadr exp) f))
              (cons name param-values)))
      (let ((definitions (cadr exp))
            (body (cddr exp)))
        (cons (make-lambda (map car definitions) body)
              (map cadr definitions)))))

; test:
(let->combination
 '(let fib-iter ((a 1)
                (b 0)
                (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

(define (let? exp)
  (tagged-list? exp 'let))
