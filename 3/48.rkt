#lang racket

; Deadlock happens when two or more processes want two
; or more of the same resources, but get them in a different
; order so that they both need to acquire resources that have
; already been claimed by the other processes.  Adding an id
; to the resources and acquiring them in order of id avoids
; this problem by ensuring that the processes try to get the
; same resources in the same order.

; to generate monotonically increasing ids for accounts
(define (monotonic-increaser)
  (let ((n 0)
        (mutex (make-mutex)))
    (define (f)
      (mutex 'acquire)
      (set! n (+ n 1))
      (let ((r n))
        (mutex 'release)
        n))
    f))

(define (make-account-and-serializer balance id-generator)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
        (id (id-generator)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) id)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id)))
    (if (< id1 id2)
        ((serializer2 (serializer1 exchange)) account1 account2)
        ((serializer1 (serializer2 exchange)) account1 account2))))