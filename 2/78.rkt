#lang racket


;; Quick definitions of put and get, which were assumed to be provided.

(define *table* '())

(define (put op type item)
  (set! *table* (cons (list op type item) *table*)))

(define (get op type)
  (define (helper remaining-rows)
    (cond ((null? remaining-rows)
           (error "op/type combination not found in table"))
          ((and (equal? op (caar remaining-rows))
                (equal? type (cadar remaining-rows)))
           (caddar remaining-rows))
          (else
           (helper (cdr remaining-rows)))))
  (helper *table*))

;; Exercise 2.78

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "[type-tag] Bad tagged datum" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "[contents] Bad tagged datum" datum))))

;; Testing on the actual arithmetic package

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "[apply-generic] No method for these types" (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)

(add ((get 'make 'scheme-number) 3) ((get 'make 'scheme-number) 5))