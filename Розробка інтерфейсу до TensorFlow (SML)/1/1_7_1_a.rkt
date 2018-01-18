#lang racket
(define (fun x y z)
  (define (afun c d)
    (/ (- (sqrt(abs (- c 1))) (expt d (/ 1 3))) (+ 1 (/ (sqr c) 2) (/ (sqr d) 4)))
    )
  (define (bfun l f)
    (* l (+ (atan f) (expt (exp 1) (* (- 1.0) (+ l 3)))))
  
  )
  (list 'a= (afun x y) 'b= (bfun y z))
  )
(error-escape-handler (lambda ()
  (error "Что-то пошло не так"))
 )