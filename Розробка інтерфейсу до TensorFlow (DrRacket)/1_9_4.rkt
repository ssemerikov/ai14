#lang racket
(define (par x)
  (if (integer? x)
      (if (= (remainder x 2) 0)
          "Четное"
          "Нечетное")
      "Некорректные данные!"
      )
  )