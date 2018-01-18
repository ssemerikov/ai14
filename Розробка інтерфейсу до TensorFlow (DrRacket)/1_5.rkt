#lang racket
(define (f x y)
  (define (d1 a b)
         (- 3 (* a b)))
  (define (d2 c)
         (+ 5 c))
  (+ (* x (d1 x y)) (* y (d2 x) (d2 x) (d2 x)) (* (d1 x y) (d2 x)))
  )