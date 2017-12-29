#lang racket

(require srfi/25)
(require net/url)
(require racket/block)
;(define (finder html [what "table scrollabletable"])
;  (cond
;    ((null? html) '())
;    (
;     (equal? (car html) what)
;     (cdr html)
;     )
;    (
;     (list? (car html))
;     (finder (car html) what)
;     )
;    (else
;     (finder (cdr html) what)
;     )
;    )
;  )

(define (getlist data [begin "<table class="] [end "</table"])
  (do ((i 0 (+ i 1)) (res '()) (flag #f) )
    ((>= i (length data)) res)
    (when (string-contains?  (list-ref data i) begin)
      (set! flag #t)
      )
    (when (string-contains?  (list-ref data i) end)
      (set! flag #f)
      (set! res (append res (list (list-ref data i))))
      )
    (when flag
      (set! res (append res (list (list-ref data i))))
      )
    )
  )

;(define (getlist1 data [begin  "<td class="] [end " <td class=\"_pic scrollabletable__td__fixed\">"])
;  (do ((i 0 (+ i 1)) (res '()) (flag #f) )
;    ((>= i (length data)) res)
;    (when (string-contains?  (list-ref data i) begin)
;      (set! flag #t)
;      )
;    (when (string-contains?  (list-ref data i) end)
;      (set! flag #f)
;      )
;    (when flag
;      (set! res (append res (list (list-ref data i))))
;      )
;    )
;  )

(define (getnames data)
  (do ((data data (cdr data)) (str (car data) (car data))  (res '())  )
    ((null? data) res)
    (when (string-contains? str  "_big scrollabletable__td__fixed")
      (set! res (append res (list (between str "/result.html\">" "</a>"))))
      )
    )
  )

(define (getscores data)
  (do ((data data (cdr data)) (str (car data) (car data))  (res '())  )
    ((null? data) res)
    (when (and (string-contains? str  ":") (not (string-contains? str  "http")))
      ;(set! res (append res (list str)))
      (if (string-contains? str  "–:–")
          (set! res (append res (list (list #f #f))))
          (let ((p1 (string-trim (first (string-split str ":")))) (p2 (second (string-split str ":"))))
            (set! res (append res (list (list (string->number p1) (string->number (string-trim (substring p2 0 3)))))))
            )
          )
      )
    )
  )


(define (between str begin end)
  (if (not (string-contains? str begin))
      ""
      (let ((bpos 0) (epos 0) )
        (do ( (i 0 (+ i 1)) (res "") )
          ((or (>= i (- (string-length str) (string-length begin) )) (string=? res begin)) (set! bpos ( -(+ i (string-length begin))1)))
          (set! res (substring str i (+ i (string-length begin))))
          ;      str i (+ i (string-length begin))
          )
        (set! str (substring str bpos))
        (if (not (string-contains? str end))
            ""
            (do ( (i 0 (+ i 1)) (res "") )
              ((or (>= i (- (string-length str) (string-length end) )) (string=? res end)) (set! epos ( - i 1)) (substring str 0 epos))
              (set! res (substring str i (+ i (string-length end))))
              ;      str i (+ i (string-length begin))
              )
            
            )
        )
      )
  )



(define (make-matrix m n)
  (if (and (integer? m) (positive? m))
      (make-array (shape 0 m 0 n) 0)
      (error "Недозволенная размерность матрицы")
      )
  )

(define (print-matrix lst m)
  (cond ((file-exists? "etalons.txt") (delete-file "etalons.txt"))
        ((file-exists? "teams.txt") (delete-file "teams.txt")))
  (with-output-to-file "teams.txt" 	#:mode 'text  #:exists 'replace
 (lambda () (print  (first gdata) )))
  (do [(i 0 (+ 1 i))] ((= i (array-end m 0)) (printf "\n"))
   
    (do [(j 0 (+ 1 j))] ((= j (array-end m 0)) (printf "\n"))
        
  (block(cond ;((file-exists? "etalons.txt") (delete-file "etalons.txt"))
          ( (not(equal?(array-ref m i j) '(#f #f))) (with-output-to-file "etalons.txt" 	#:mode 'text  #:exists 'append
    (lambda () (printf "~a ~a~n~a ~a ~n"  i j (car(array-ref m i j))(cadr(array-ref m i j))))))))
;      (cond  ((not(equal?(array-ref m i j) '(#f #f))) (with-output-to-file "etalons.txt" 	#:mode 'text #:exists 'append
;    (lambda () (printf "~a ~a~n"  (car(array-ref m i j))(cadr(array-ref m i j))))))))
      ;(printf "~S " (array-ref m i j))
      )
    )
  )


(define (getgames url)
  (let*
      (
       (f (get-pure-port (string->url url)))
       (rawdata (getlist (port->lines f)))
       (names (getnames rawdata)) 
       (cmdcnt (length names))
       (games (make-matrix cmdcnt cmdcnt))
       )

    (close-input-port f)

    (do ((i 0 (+ i 1)) (rawscores (getscores (getlist rawdata "<td class=\"_res\">" "</td>"))) (pos 0 ) )
      ((= i cmdcnt) (list names games))
      (do ((j 0 (+ j 1)))
        ((= j cmdcnt) )
        (cond
          (
           (= i j)
           (array-set! games i j (list #f #f))
           )
          (else
           (array-set! games i j (list-ref rawscores pos))
           (set! pos (+ pos 1))
           )
          )
        )
      )
    )
  )

(define gdata (getgames "https://www.championat.com/football/_england/2214/result.html"))
(print-matrix (first gdata) (second gdata))
