#lang r5rs

;; impl of error from http://stackoverflow.com/questions/3120379/drracket-r5rs-and-the-error-procedure
;;; create binding for error
(define error #f)

;;; capture toplevel continuation
;;;  assign a function to error, allowing a variable number of arguments to
;;;  be passed
(call-with-current-continuation (lambda (k)
              (set! error
                (lambda error-arguments
                  (display ">>>> ERROR ")
                  (newline)
                  (k error-arguments)))
              'done))

;; queue code from book

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called on an empty queue")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue"))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; print-queue

(define (print-queue queue)
  (display (car queue)))

;; explanation:
;
; Our queues are represented as pairs whose car points to the beginning of the
; queue and whose cdr points to the end of the queue.  So what we expect to see
; printed when we print the queue is actually just the car of the queue, which
; points to the first pair in the list.  But when we print the queue, the
; interpreter only knows it as a pair, so it first prints the car of the pair
; (which is a list that is what we want when we print the queue) and then it
; prints the cdr of the pair (which merely points to the final item in the
; queue).  This is why Ben sees the last item printed twice: it's printed once
; at the end of the list that represents the queue (the car of the queue), and
; again as the value at the rear pointer of the queue (the cdr of the queue).

