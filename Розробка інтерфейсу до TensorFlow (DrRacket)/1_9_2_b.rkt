#lang racket
(define (min x y)
  (if (and (or (integer? x) (real? x)) (or (integer? y) (real? y)))
      (if (< x y) x
          (if (= x y)
              "Невозможно найти минимальное число, так как они равны!"
              y))
      "Некорректные данные!"
      )
  )