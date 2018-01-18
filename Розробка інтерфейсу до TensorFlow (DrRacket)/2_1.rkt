#lang racket
;
(define (x a b)
  (if (and (or (integer? a) (real? a)) (or (integer? b) (real? b)))
      (if (> a b)
          (if (= a 0)
              "Число а не может быть равно 0"
              (+ (/ b a) 1))
          (if (= a b)
              (- a 5)
              (if (= b 0)
                  "Число b не может быть равно 0"
                  (/ (- (* 3 b) (* 5 a)) b)
                  ))
          )
      "Некорректный ввод"
      )
  )
  

