#lang racket

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))
  
(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-struct branch)
  (cadr branch))

(define (total-weight mobile)
  (cond ((number? mobile) mobile)
        (else (+ (total-weight (branch-struct (left-branch mobile)))
                 (total-weight (branch-struct (right-branch mobile)))))))

(define m1 (make-mobile (make-branch 3 (make-mobile (make-branch 4 2)
                                                    (make-branch 2 4)))
                        (make-branch 9 2)))

(total-weight m1)

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-struct branch))))

(define (balanced? mobile)
  (cond ((number? mobile) true)
        (else (and (= (torque (left-branch mobile))
                      (torque (right-branch mobile)))
                   (balanced? (branch-struct (left-branch mobile)))
                   (balanced? (branch-struct (right-branch mobile)))))))

(balanced? m1)

; To support the change suggested in part (d), I would only need to change
; the cadr's of right-branch and branch-struct to cdr's. Importantly, I
; wouldn't need to change the algorithmic functions total-weight, torque,
; or balanced?.
