#lang racket
(define (f x y)
  (define (cub a)
    (* a a a))
  (- (* 2 (cub x)) (* 3 (cub y)))
  )