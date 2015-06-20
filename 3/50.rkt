#lang racket

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car-stream argstreams))
       (apply stream-map (cons proc (map cdr-stream argstreams))))))
