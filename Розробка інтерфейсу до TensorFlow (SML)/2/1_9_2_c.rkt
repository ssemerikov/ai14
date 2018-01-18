#lang racket
(define (z x y)
  (if (and (or (integer? x) (real? x)) (or (integer? y) (real? y)))
      (if (> x y) (- x y)
          (+ (- y x) 1)
          )
      "Некорректные данные!"
      )
  )