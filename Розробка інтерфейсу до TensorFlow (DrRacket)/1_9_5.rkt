#lang racket
(define (hun x)
  (if (integer? x)
      (if(> x 99)
         (quotient x 100) ;(quotient x y)- ціла частина від ді-лення х на у
         "Число меньше 100"
         )
      "Некорректные данные!"
      )
  )