V#lang racket
(define (R r1 r2 r3)
  (define (op x)
    (/ 1 x))
  (/ 1 (+ (op r1) (op r2) (op r3)) )
  )