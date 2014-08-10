#lang racket

(define (call-the-cops)
  (error "WEEEE-OOOOOOO WEEEEEE-OOOOOOOO WEEEEE-OOOOOOO"))

(define (make-account balance password)
  (define consecutive-password-failures 0)
  (define (withdraw amount)
    (if (< balance amount)
        "insufficient funds"
        (begin (set! balance (- balance amount))
                     balance)))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m password-input)
    (if (not (eq? password password-input))
        (lambda (x)
          (set! consecutive-password-failures (+ 1 consecutive-password-failures))
          (if (> consecutive-password-failures 7)
              (call-the-cops)
              "wrong password"))
        (begin
          (set! consecutive-password-failures 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" m))))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'withdraw 'wrong-password) 50)

((acc 'withdraw 'secret-password) 50)

((acc 'deposit 'wrong-password) 14)

((acc 'deposit 'secret-password) 14)

((acc 'withdraw 'a) 1)
((acc 'withdraw 'b) 2)
((acc 'withdraw 'c) 3)
((acc 'withdraw 'd) 4)
((acc 'withdraw 'e) 5)
((acc 'withdraw 'f) 6)
((acc 'withdraw 'g) 7)
((acc 'withdraw 'h) 8)
