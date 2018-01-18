#lang racket
(define (fun x y z)
  (define (afun c d e)
    (/
     (+ 3 (expt (exp 1) (- d 1))) (+ 1 (* c c) (abs (- d (tan e)))))
    )
    
  (define (bfun l f)
    (+
     1 (abs(- f l)) (/ (* (- f l) (- f l)) 2) (/ (* (- f l) (- f l) (- f l)) (* 3 (sin l))))
  )
  (list 'a= (afun x y z) 'b= (bfun x y))
  )