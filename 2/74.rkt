#lang racket

;; Totally untested.

;; a.

(define (make-file division orig-file)
  (list division orig-file))

(define (file-division file)
  (car file))

(define (file-contents file)
  (cadr file))

(define (get-record employee file)
  ((get 'get-record (file-division file)) employee (file-contents file)))

;; b.

(define (get-salary employee file)
  ((get 'get-salary (file-division record)) employee (get-record employee file)))

;; c.

(define (find-employee-record employee files)
  (if (null? files)
      "no record found"
      (let ((record (get-record employee (car files))))
        (or record (find-employee-record employee (cdr files))))))
