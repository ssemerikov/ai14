#lang racket/gui



;******************************Інтерфейс*******************************************************
;**********************************************************************************************
;============================ВІКНО=============================================================
(define Вікно (new frame% 
                   [label "Розпізнавання масті карти по зображенню"]
                   [width 540] 
                   [height 300] 
                   [x 100] 
                   [y 150] 
                   [style '(no-system-menu metal)]
                   [alignment '(center center)] 
                   ))


;=============================ПАНЕЛЬ-ОСНОВНА===================================================
(define Панель (new horizontal-panel%
                    [parent Вікно]
                    [min-width 540]
                    [min-height 300]
                    [alignment '(center center)]
                    ))
;=============================ПАНЕЛЬ-ЗЛІВА=====================================================
(define Панель-зліва (new vertical-panel%
                          [parent Панель]
                          [min-width 240] 
                          [min-height 300] 
                          [alignment '(center center)]
                          ))
;=============================ПАНЕЛЬ-СПРАВА====================================================
(define Панель-справа (new vertical-panel%
                           [parent Панель]
                           [min-width 20]
                           [min-height 20] 
                           [alignment '(center center)] 
                           ))
(define Повідомлення (new message%       [parent Вікно] 

 
      [label "                                Оберіть пункт меню                              "]))


;=============================КНОПКА-1.=========================================
(define Кнопка-навчання (new button% 
                               [parent Панель-зліва] 
                               [min-width 250]	 
                               [min-height 50]
                               [vert-margin 10]
                               [horiz-margin 200] 
                               [callback (lambda (button event) (begin 
(main) 
(send Повідомлення set-label "Навчання успішно завершено") 
))]
                               [label "Навчання мережі"]))
(define Кнопка-завантаження (new button% 
                               [parent Панель-зліва] 
                               [min-width 250]	 
                               [min-height 50]
                               [vert-margin 10]
                               [horiz-margin 10] 
                               [callback (lambda (button event) (begin 
(read-network "network.txt") 
(send Повідомлення set-label "Коефіцієнти завантажено") 
))]                            [label "Завантаження вагових коефіціентів"]))
(define Кнопка-розпізнати (new button% 
                               [parent Панель-зліва] 
                               [min-width 250]	 
                               [min-height 50]
                               [vert-margin 10]
                               [horiz-margin 10] 
                               [callback (lambda (button event) (send Рамка show #t))]
   [label "Розпізнавання масті"]))

(define Рамка (new frame%
                   [label "Вхідні дані"]
                   [width 300]
                   [height 100]))
(define Поле (new text-field% 
                  [label "Введіть назву зображення:"]
                  [min-width 300]
                  [parent Рамка]
                  ))
(define Прийняти (new button% 
                      [parent Рамка] 
                      [label "Розпізнати масть"] 
                      [min-width 200] 
                      [callback (lambda (button event) 
                                  (define x1  (send Поле get-value))
                                  ;(define listtt  '())
                                  (set! z "")
                                  (do ((i 0 (+ i 1)))
                                    ((= i (- (string-length x1) 0)))
                                     (when (not (equal? (string-ref x1 i) #\space))
                                       (set! z (string-append z (string (string-ref x1 i))))
                                        ; (set! listtt (append listtt (list (string->number (string (string-ref x1 i))))))
                                     
                                         )
                                     )
                       
                                      ;(set! z (list->vector listtt)) 
                                 (bmprec z)
                                  )]))

(define z "") 

;=============================КНОПКА-2==========================================
(define Кнопка-вихід (new button% 
                          [parent Панель-зліва] 
                          [min-width 250]	 
                          [min-height 50] 
                          [vert-margin 10] 
                          [horiz-margin 10] 
                          [callback (lambda (button event) (send Вікно on-exit))]
                          [label "Вихід"]))
(send Вікно show #t) ;відображення вікна
;*******************************************************************************************************************************************************************
;************************************************************************Інтерфейс**********************************************************************************
;*******************************************************************************************************************************************************************



(require srfi/25)


(define (1+ n)

  (+ 1 n)

  )


;трехлойная нейронная сеть (вход, скрытый слой, выход


(define NUMIN 0)  ;размерность входа

(define NUMHID 0) ;размерность скрытого слоя

(define NUMOUT 0) ;размерность выхода


;матрицы весовых коэффициентов

(define WeightIH null) ;соединения входа и скрытого слоя

(define WeightHO null) ;соединения скрытого слоя и выходa


;для нормализации

(define mini 0)

(define maxi 0)

(define mino 0)

(define maxo 0)

(define GlobalMinError 100000000)


;функции для создания векторов и матриц


(define (make-vector size)

  (if (and (integer? size) (positive? size))

          (make-array (shape 0 size) 0)

          (error "Недозволенная размерность вектора")

          )

  )


(define (make-matrix m n)

  (if (and (integer? m) (positive? m))

          (make-array (shape 0 m 0 n) 0)

          (error "Недозволенная размерность матрицы")

          )

  )


(define (vector-from-matrix m index)

  (define res (make-vector (array-end m 1)))

  (do [(i 0 (+ 1 i))] ((= i (array-end m 1)) res)

        (setvvalue res i (getmvalue m index i))

        )

  )


(define (print-vector v)

  (do [(i 0 (+ 1 i))] ((= i (array-end v 0)) (printf "\n"))

        (printf "~S " (getvvalue v i))

        )

  )

;функции для получения/установки значений векторов и матриц


(define (getvvalue obj i)

  (array-ref obj i)

  )



(define (getmvalue obj i j)

  (array-ref obj i j)

  )



(define (setvvalue vec i val)

  (array-set! vec i val)

  )


(define (setmvalue matrix i j val)

  (array-set! matrix i j val)

  )


(define (rando)

  (/ (random 32000) 32000.0)

  )



;создание нейронной сети


(define (make-network _NUMIN _NUMOUT _NUMHID)

  (set! NUMIN _NUMIN)

  (set! NUMOUT _NUMOUT)

  (set! NUMHID _NUMHID)

 

  (set! WeightIH (make-matrix (+ 1 NUMIN) NUMHID))

  (set! WeightHO (make-matrix (+ 1 NUMHID) NUMOUT))

 

  (define smallwt 0.5)

 

  ;устанавливаем случайные весовые коэффициенты

 

  (do [(j 0 (+ 1 j))] ((= j NUMHID))

        (do [(i 0 (+ 1 i))] ((= i (+ 1 NUMIN)))

          (setmvalue WeightIH i j  (* 2.0 (- (rando) 0.5) smallwt))

          )

        )

 

  (do [(k 0 (+ 1 k))] ((= k NUMOUT))

        (do [(j 0 (+ 1 j))] ((= j (+ 1 NUMHID)))

          (setmvalue WeightHO j k (* 2.0 (- (rando) 0.5) smallwt))

          )

        )

  )




;обучение нейронной сети

(define (train TrainInput TrainTarget Err MaxCount DoOut NetworkFile)

  (let* (

             [Error (+ Err 1)]

             [eta 0.5]

             [alpha 0.9]

             [NUMPAT (array-end TrainInput 0)] ;число обучающих шаблонов

             [ranpat (make-vector NUMPAT)]

             [NumPattern NUMPAT]

             [NumInput NUMIN]

             [NumHidden NUMHID]

             [NumOutput NUMOUT]

             ;временные массивы

             [DeltaWeightIH (make-matrix (1+ NUMIN) NUMHID)]

             [DeltaWeightHO (make-matrix (1+ NUMHID) NUMOUT)]

             [SumDOW (make-vector NUMHID)]

             [DeltaH (make-vector NUMHID)]

             [DeltaO (make-vector NUMOUT)]

             [SumH (make-vector NUMHID)]

             [Hidden (make-vector NUMHID)]

             [SumO (make-vector NUMOUT)]

             [Output (make-vector NUMOUT)]

             [Input (make-matrix NUMPAT NUMIN)]

             [Target (make-matrix NUMPAT NUMOUT)]

             )

   

        ;копируем тренировочные матрицы во временные во избежание порчи

        (do [(i 0 (+ 1 i))] ((= i NUMPAT))

          (do [(k 0 (+ 1 k))] ((= k NUMIN))

            (setmvalue Input i k (getmvalue TrainInput i k))

            )

          )

        (do [(i 0 (+ 1 i))] ((= i NUMPAT))

          (do [(k 0 (+ 1 k))] ((= k NUMOUT))            

            (setmvalue Target i k (getmvalue TrainTarget i k))

            )

          )

   

        ;если существует файл NetworkFile - загрузим его для дообучения

        (cond

          [(file-exists? NetworkFile) (read-network NetworkFile)]

          [#t

           (set! mini (getmvalue Input 0 0))

           (set! maxi (getmvalue Input 0 0))

           (set! mino (getmvalue Target 0 0))

           (set! maxo (getmvalue Target 0 0))

   

           ;поиск граничных значений в числовых массивах

           (do [(i 0 (+ 1 i))] ((= i NumPattern))

             (do [(k 0 (+ 1 k))] ((= k NumInput))            

               (when (> mini (getmvalue Input i k))

                 (set! mini (getmvalue Input i k))

                 )

               (when (< maxi (getmvalue Input i k))

                 (set! maxi (getmvalue Input i k))

                 )

               )

             (do [(k 0 (+ 1 k))] ((= k NumOutput))            

               (when (> mino (getmvalue Target i k))

                 (set! mino (getmvalue Target i k))

                 )

               (when (< maxo (getmvalue Target i k))

                 (set! maxo (getmvalue Target i k))

                 )

               )

             )

           ]

          )

   

        ;нормализация

        (do [(i 0 (+ 1 i))] ((= i NumPattern))

          (do [(k 0 (+ 1 k))] ((= k NumInput))            

            (setmvalue Input i k

                       (/ (- (getmvalue Input i k) mini) (- maxi mini))

                       )

            )

          (do [(k 0 (+ 1 k))] ((= k NumOutput))            

            (setmvalue Target i k

                       (/ (- (getmvalue Target i k) mino) (- maxo mino))

                       )

            )

          )


   

        ;цикл обучения по достижению заданной ошибки или числа итераций

        (do [(epoch 0 (+ 1 epoch)) ] ( (or (= epoch MaxCount) (< Error Err)) (write-network NetworkFile) Error)            

          ;перемешиваем шаблоны

          (do [(p 0 (+ 1 p))] ((= p NumPattern))

            (setvvalue ranpat p (random NumPattern))

            )

          (set! Error 0.0)

         

          ;цикл обучения по шаблонам

          (do [(np 0 (+ 1 np)) (p (getvvalue ranpat 0) (getvvalue ranpat np))] ((= np NumPattern))                

            ;выбираем шаблон

            ;активация скрытого слоя

            (do [(j 0 (+ 1 j))] ((= j NumHidden))                

              (setvvalue SumH j (getmvalue WeightIH 0 j))

              (do [(i 0 (+ 1 i))] ((= i NumInput))

                (setvvalue SumH j

                           (+ (getvvalue SumH j)

                              (*

                               (getmvalue Input p i)

                               (getmvalue WeightIH (1+ i) j)

                               )

                              )

                           )

                )

              (setvvalue Hidden j (/ 1.0

                                     (+

                                      1.0

                                      (exp (- (getvvalue SumH j)))

                                      )    

                                     )

                         )

              )

            ;активация выходного слоя и вычисление ошибки

            (do [(k 0 (+ 1 k))] ((= k NumOutput))            

              (setvvalue SumO k (getmvalue WeightHO 0 k))

              (do [(j 0 (+ 1 j))] ((= j NumHidden))                

                (setvvalue SumO k (+

                                   (getvvalue SumO k)

                                   (*

                                    (getvvalue Hidden j)

                                    (getmvalue WeightHO (1+ j) k)

                                    )

                                   )

                           )

                )

              ;сигмоидальный вывод

             

              (setvvalue Output k (/ 1.0

                                     (+

                                      1.0

                                      (exp (- (getvvalue SumO k)))

                                      )    

                                     )

                         )

              (set! Error (+

                           Error

                           (*

                            0.5

                            (- (getmvalue Target p k) (getvvalue Output k))

                            (- (getmvalue Target p k) (getvvalue Output k))

                            )

                           )

                    )

              (setvvalue DeltaO k

                         (*

                          (- (getmvalue Target p k) (getvvalue Output k))

                          (getvvalue Output k)

                          (- 1.0 (getvvalue Output k))

                          )

                         )

              )

            ;обратное распространение ошибки на скрытый слой

            (do [(j 0 (+ 1 j))] ((= j NumHidden))                

              (setvvalue SumDOW j 0.0)

              (do [(k 0 (+ 1 k))] ((= k NumOutput))            

                (setvvalue SumDOW j

                           (+

                            (getvvalue SumDOW j)

                            (*

                             (getmvalue WeightHO (1+ j) k)

                             (getvvalue DeltaO k)

                             )

                            )

                           )

                )

              (setvvalue DeltaH j

                         (*

                          (getvvalue SumDOW j)

                          (getvvalue Hidden j)

                          (- 1.0 (getvvalue Hidden j))

                          )

                         )

              )

            (do [(j 0 (+ 1 j))] ((= j NumHidden))                

              (setmvalue DeltaWeightIH 0 j

                         (+

                          (*

                           eta

                           (getvvalue DeltaH j)

                           )

                          (*

                           alpha

                           (getmvalue DeltaWeightIH 0 j)

                           )

                          )

                         )

              (setmvalue WeightIH 0 j

                         (+

                          (getmvalue WeightIH 0 j)

                          (getmvalue DeltaWeightIH 0 j)

                          )

                         )

              (do [(i 0 (+ 1 i))] ((= i NumInput))

                (setmvalue DeltaWeightIH (1+ i) j

                           (+

                            (*

                             eta

                             (getmvalue Input p i)

                             (getvvalue DeltaH j)

                             )

                            (*

                             alpha

                             (getmvalue DeltaWeightIH (1+ i) j)

                             )

                            )

                           )

                (setmvalue WeightIH (1+ i) j

                           (+

                            (getmvalue WeightIH (1+ i) j)

                            (getmvalue DeltaWeightIH (1+ i) j)

                            )

                           )

                )

              )

            (do [(k 0 (+ 1 k))] ((= k NumOutput))            

              (setmvalue DeltaWeightHO 0 k

                         (+

                          (*

                           eta

                           (getvvalue DeltaO k)

                           )

                          (*

                           alpha

                           (getmvalue DeltaWeightHO 0 k)

                           )

                          )

                         )

              (setmvalue WeightHO 0 k

                         (+

                          (getmvalue WeightHO 0 k)

                          (getmvalue DeltaWeightHO 0 k)

                          )

                         )

              (do [(j 0 (+ 1 j))] ((= j NumHidden))                

                (setmvalue DeltaWeightHO (1+ j) k

                           (+

                            (*

                             eta

                             (getvvalue Hidden j)

                             (getvvalue DeltaO k)

                             )

                            (*

                             alpha

                             (getmvalue DeltaWeightHO (1+ j) k)

                             )

                            )

                           )

                (setmvalue WeightHO (1+ j) k

                           (+

                            (getmvalue WeightHO (1+ j) k)

                            (getmvalue DeltaWeightHO (1+ j) k)

                            )

                           )

                )

              )

            )

          (when (and DoOut (= (remainder epoch 20) 0));отладочный вывод
     
            (printf "epoch=~S, error=~S\n" epoch Error)

            )

          (when (< Error GlobalMinError)

            (set! GlobalMinError Error)

            (printf "epoch=~S, (min)error=~S\n" epoch Error)

            (write-network NetworkFile)

            )

          )

        )

  )


;подача сигнала на вход сети и получение результата

(define (getoutput BeInput

                       )

  (let (

            [Input (make-vector NUMIN)]

            [Output (make-vector NUMOUT)]

            [result (make-vector NUMOUT)]

            [SumH (make-vector NUMHID)]

            [Hidden (make-vector NUMHID)]

            [SumO (make-vector NUMOUT)]

            [NumInput NUMIN]

            [NumHidden NUMHID]

            [NumOutput NUMOUT]

            )

   

        ;нормализация входа

        (do [(k 0 (+ 1 k))] ((= k NumInput))            

          (setvvalue Input k (/

                              (- (getvvalue BeInput k) mini)

                              (- maxi mini)

                              )

                     )

          )

   

        ;активация скрытого слоя

        (do [(j 0 (+ 1 j))] ((= j NumHidden))                

          (setvvalue SumH j (getmvalue WeightIH 0 j))

          (do [(i 0 (+ 1 i))] ((= i NumInput))

            (setvvalue SumH j

                       (+ (getvvalue SumH j)

                          (*

                           (getvvalue Input i)

                           (getmvalue WeightIH (1+ i) j)

                           )

                          )

                       )

            )

          (setvvalue Hidden j (/ 1.0

                                 (+

                                  1.0

                                  (exp (- (getvvalue SumH j)))

                                  )    

                                 )

                     )

          )

   

        ;активация выходного слоя

        (do [(k 0 (+ 1 k))] ((= k NumOutput))            

          (setvvalue SumO k (getmvalue WeightHO 0 k))

          (do [(j 0 (+ 1 j))] ((= j NumHidden))                

            (setvvalue SumO k (+

                               (getvvalue SumO k)

                               (*

                                (getvvalue Hidden j)

                                (getmvalue WeightHO (1+ j) k)

                                )

                               )

                       )

            )

          (setvvalue Output k (/ 1.0

                                 (+

                                  1.0

                                  (exp (- (getvvalue SumO k)))

                                  )    

                                 )

                     )

          )

   

        ;денормализация выхода

        (do [(k 0 (+ 1 k))] ((= k NumOutput) result)            

          (setvvalue result k (+

                               (*

                                (getvvalue Output k)

                                (- maxo mino)

                                )

                               mino

                               )

                     )

          )

        ))


;пример создания использования нейронной сети


(define NUMPAT 60) ;число обучающих шаблонов - может переопределяться в файле

;(set! NUMIN  4)  ;размерность входа - может переопределяться в файле

;(set! NUMOUT 1)  ;размерность выхода - может переопределяться в файле


(define (main)

 

  ;форматы файлов матриц: число_строк число_столбцов данные

  (define f (open-input-file "etalons.txt" #:mode 'text))
  (set! NUMPAT (read f))
  (set! NUMIN (read f))
  (set! NUMOUT (read f))
  (set! NUMHID (+ (* NUMIN 2) 1)) ;число нейронов в скрытом слое

  ;(set! NUMHID 2)

  (define Input (make-matrix NUMPAT NUMIN))
  (define Output (make-matrix NUMPAT NUMOUT))
 

  (do [(i 0 (+ 1 i))] ((= i NUMPAT))
        (do [(k 0 (+ 1 k))] ((= k NUMIN))            
          (setmvalue Input i k (read f))
          )
        (do [(k 0 (+ 1 k))] ((= k NUMOUT))            
          (setmvalue Output i k (read f))
          )
        )
 
  (close-input-port f)
  (make-network NUMIN NUMOUT NUMHID)
  (define in (make-vector NUMIN))
  (define res (make-vector NUMOUT))
  (define out (make-vector NUMOUT))

 
  (printf
   "Размерность входа - ~S, размерность выхода - ~S, число шаблонов - ~S\n"
   NUMIN NUMOUT NUMPAT
   )
 
  (train Input Output 0.00001 150000 #t "network.txt")
 ; (read-network "network.txt")

 

  (printf "Исходные данные:\n")

  (do [(i 0 (+ 1 i)) (res null)] ((= i NUMPAT))

        (printf "Вход: ")

        (print-vector (vector-from-matrix Input i))

        (set! res (getoutput (vector-from-matrix Input i)))

        (printf " Эталонный выход: ")

        (print-vector (vector-from-matrix Output i))

        (printf " Полученный выход: ")

        (print-vector res)

        (when (zero? (remainder i 10))

          (printf "Press Enter to continue")

          (read-char)

          )

        )

 

  (printf "Тестовые данные:\n")

  (set! f (open-input-file "test.txt" #:mode 'text))


  (define count (read f))

  (set! NUMIN (read f))

  (do [(i 0 (+ 1 i)) (res null)] ((= i count))

        (do [(k 0 (+ 1 k))] ((= k NUMIN))            

          (setvvalue in k (read f))

          )

        (printf "Вход: ")

        (print-vector in)

        (set! res (getoutput in))

        (printf " Полученный выход: ")

        (print-vector res)

        (when (zero? (remainder i 10))

          (printf "Press Enter to continue")

          (read-char)

          )

        )

  (close-input-port f)

  )



;с помощью этих функций можно сохранять и

;загружать матрицы весовых коэффициентов

;=====================================

;


(define (read-network filename)

  (define f (open-input-file filename  #:mode 'text))

  (set! NUMIN (read f))

  (set! NUMOUT (read f))

  (set! NUMHID (read f))


  (make-network NUMIN NUMOUT NUMHID)


  (do [(i 0 (+ 1 i))] ((= i (+ 1 NUMIN)))

        (do [(k 0 (+ 1 k))] ((= k NUMHID))            

          (setmvalue WeightIH i k (read f))

          )

        )


  (do [(i 0 (+ 1 i))] ((= i (+ 1 NUMHID)))

        (do [(k 0 (+ 1 k))] ((= k NUMOUT))            

          (setmvalue WeightHO i k (read f))

          )

        )

  (set! mini (read f))

  (set! maxi (read f))

  (set! mino (read f))

  (set! maxo (read f))

  (set! GlobalMinError (read f))

 

  (close-input-port f)

  )


(define (write-network filename)

  (define f (open-output-file filename  #:mode 'text #:exists 'replace))


  (fprintf f "~S ~S ~S\n" NUMIN NUMOUT NUMHID)


  (do [(i 0 (+ 1 i))] ((= i (+ 1 NUMIN)))

        (do [(k 0 (+ 1 k))] ((= k NUMHID) (fprintf f "\n"))            

          (fprintf f "~S " (getmvalue WeightIH i k))

          )

        )


  (do [(i 0 (+ 1 i))] ((= i (+ 1 NUMHID)))

        (do [(k 0 (+ 1 k))] ((= k NUMOUT) (fprintf f "\n"))            

          (fprintf f "~S " (getmvalue WeightHO i k))

          )

        )


  (fprintf f " ~S ~S " mini maxi)

  (fprintf f " ~S ~S " mino maxo)

  (fprintf f " ~S" GlobalMinError)

 

  (close-output-port f)

  )



(define (bmprec z)


(define size (file-size z))
   (read-network "network.txt")
  
 (define what (open-input-file z #:mode 'binary))
  (define f (open-output-file "text.txt" #:mode 'binary   #:exists 'replace) )
    (fprintf f "1 2030\r\n")

         (for ([element (bytes->list (read-bytes size what))])
           
           (fprintf f "~A " element) 
      )
  
  (close-output-port f)
    (define p (open-input-file "text.txt" #:mode 'text))
  (set! NUMPAT (read p))
  (set! NUMIN (read p))
  
  ;(set! NUMHID 2)

  (define Input (make-matrix NUMPAT NUMIN))
  

  (do [(i 0 (+ 1 i))] ((= i NUMPAT))
        (do [(k 0 (+ 1 k))] ((= k NUMIN))            
          (setmvalue Input i k (read p))
          )
  
  (close-input-port p)

 

  (if (= 0 NUMHID)
      (print "Network wasn't loaded or created")
        
      (let ( (in (make-vector  NUMIN)) (res 0)  )
        (set! res (getoutput (vector-from-matrix Input 0)))
   
        (newline)
        (message-box "Полученный выход :" (format  "За розрахунком отримано такі ймовірності:~n Бубна: ~a Піка: ~a Чирва: ~a Хрест: ~a"  (array-ref res 0) (array-ref res 1 )(array-ref res 2 )(array-ref res 3 )) #f '(ok)) ;(printf (vector-from-matrix res -1)))
        (print-vector res)
        (newline)
        
        )
      )
  
))
;(bmprec)
