#lang racket
(define (gr x)
 (number -> string x)
  (if (< (string-length x) 3)
      (cond
        ((and (= x 1)(= x 21)) (list x 'гривня))
        ((and () ())(list x 'гривні)))
        ())
  (string -> number)
  
  (/ x 100))
(define (sum kop)
  (if (number? kop)
      (if (and (>= kop 0) (<= kop 100000))
          (if (> (gr kop) 0)
              (cond (gr kop)
                  ((1 21 31 41 51 61 71 81 91) (list (gr kop) 'гривня))
                ((2 4 22 23 24 32 33 34 42 43 44 52 53 54 62 63 64 72 73 74 82 83 84 92 93 94) (list (gr kop) 'гривні)) 
                (3 4 (else "Error!"))
              "Менше 1 гривны!"
              )
          "Число вне диапазона!"
          )
      "Не корректные данные!"
      )
  (+ kop 2)
  )
