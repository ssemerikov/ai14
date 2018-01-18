#lang racket
(define (number n)
  (if (integer? n)
      (if(<= n 100)
         (cond
           (and (< n -10) (< n 10)
            (list 'count 1 'sum n 'last 'and 'first '= n))
           ((and (>= n 10) (< n 100))
            (list 'count 2 'sum (+ (remainder n 10) (quotient n 10)) 'last (remainder n 10) 'first (quotient n 10))
            )
           ((= n 100)
            (list 'count 3 'sum 1 'last 0 'first 1)))
         "Число больше 100"
         )
      "Не коррректные данные"
      )
  )
;(quotient x y)- ціла частина від ді-лення х на у
;(remainder x y) - остача від ділення х на у