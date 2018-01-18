#lang racket
(define (k a b c x)
  (define (cvad e)
    (* e e))
  (- (* a (cvad x)) (* b (x)) (c))
  )