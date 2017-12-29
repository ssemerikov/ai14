#lang racket
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
      (when (and DoOut (= (remainder epoch 10) 0));отладочный вывод
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
  ;(set! NUMHID (+ (* NUMIN 2) 1)) ;число нейронов в скрытом слое
  (set! NUMHID NUMOUT)
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
 
  (train Input Output 0.001 1000 #t "network.txt")
  ;(read-network "network.txt")
 
  (printf "Исходные данные:\n")
  (do [(i 0 (+ 1 i)) (res null) (pair null)] ((= i NUMPAT))
    (printf "Вход: ")
    (print-vector (vector-from-matrix Input i))
    (set! res (getoutput (vector-from-matrix Input i)))
    (printf " Эталонный выход: ")
    (print-vector (vector-from-matrix Output i))
    (printf " Полученный выход: ")
    (print-vector res)
    (set! pair (getpnum res))
    (printf "~nС вероятностью ~A это ~A~n" (first pair) (second pair))
    (when (zero? (remainder i 10))
      (printf "Press Enter to continue")
      (read-char)
      )
    )
 
  (printf "Тестовые данные:\n")
  (set! f (open-input-file "test.txt" #:mode 'text))
  (define count (read f))
  (set! NUMIN (read f))
  (do [(i 0 (+ 1 i)) (res null) (pair null)] ((= i count))
    (do [(k 0 (+ 1 k))] ((= k NUMIN))            
      (setvvalue in k (read f))
      )
    (printf "Вход: ")
    (print-vector in)
    (set! res (getoutput in))
    (printf " Полученный выход: ")
    (print-vector res)
    (set! pair (getpnum res))
    (printf "~nС вероятностью ~A это ~A~n" (first pair) (second pair))
    
    (when (zero? (remainder i 10))
      (printf "Press Enter to continue")
      (read-char)
      )
    )
  (close-input-port f)
  )

(define (getpnum v)
  (do ([i 0 (+ i 1)] [elem (getvvalue v 0) (getvvalue v i)] [max (getvvalue v 0)] [num 0])
    ((>= i 9) (list max num))
    (when (> elem max)
      (set! max elem)
      (set! num i) 
      )
    )
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


(define (loadmatrix filename)
  (define f (open-input-file filename))
  
  (define m (read f))
  (define n (read f))
  
  (define res (make-matrix m n ))
  
  (do [(i 0 (+ 1 i))] ((= i m))
    (do [(k 0 (+ 1 k))] ((= k n))        
      (setmvalue res i k (read f))
      )
    )
  (set! mini (read f))
  (set! maxi (read f))
  (set! mino (read f))
  (set! maxo (read f))
  
  (close-input-port f)
  res
  )

(define (loadcoeffs first second)
  (set! WeightIH (loadmatrix first))
  (set! WeightHO (loadmatrix second))
  (set! NUMIN (- (array-end WeightIH 0) 1))
  (set! NUMOUT (array-end  WeightHO 1))
  (set! NUMHID (array-end WeightIH 1))
  )

(define (readvector v)
  (do ((i 0 (1+ i)) )
    ((= i (array-end v 0))   )
    (array-set! v i (read))
    )
  )

(define список-симптомов '(
"1. Присутствует дискомфорт в груди?"
"2. Есть ли тошнота, изжога, расстройство желудка?"
"3. Боль отдающая в руку?"
"4. Головокружение или легкость в голове?"
"5. Боль в горле или челюсти?"
"6. Есть слабость, утомленность?"
"7. Тревожил храп?"
"8. Холодный пот?"
"9. Непрекращающийся кашель?"
"10. Есть ли у вас кожные проявление?"
"11. Присутствует ли у вас повышенная температура до 39-40 градусов?"
"12. Очень быстрое биение сердца?"
"13. Есть сильные ознобы?"
"14. Отеки рук, ног?"
"15. Головная боль?"
"16. Тревожила одышка?"
"17. Повышенная потливость?"
"18. Была ли у вас боль в суставах?"
"19. Сыпь?"
"20. Была потеря сознания или ощущение, близкое к обмороку?"
"21. Температура тела была нормальная?"
"22. Недостаточность кровообращения?"
"23. Лихардка? "
"24. Замечали ухудшение памяти?"

)
  )


(define (readdata v)
  (display "Опрос пациента:\n")
  (display "Введите 1, если симптом присутствует, и 0, если нет: ")
  (newline)
  (do ((i 0 (1+ i)) )
    ((= i (array-end v 0))   v)
    (display (list-ref список-симптомов i))
    (newline)
    (array-set! v i (read))
    )
     )


(define список-болезней '(
"Миокардит \n
Профилактика:
 Острая стадия миокардита требует госпитализации в отделение кардиологии, 
 ограничения физической активности, строгого постельного режима
 на 4 - 8 недель до достижения компенсации кровообращения
 и восстановления нормальных размеров сердца. 
 Диета при миокардите предполагает ограниченное употребление поваренной
 соли и жидкости, обогащенное белковое и витаминизированное 
 питание для нормализации метаболических процессов в миокарде."
"Эндокардит \n
Профилактика:
 Чтобы препятствовать развитию ИЭ, нужно стараться избегать 
 чрезмерных физических и психических нагрузок, укреплять иммунитет. 
 Важно помнить, что любой очаг хронической инфекции является потенциальной причиной эндокардита. 
 Поэтому не следует затягивать с лечением даже самых банальных инфекций
 ,таких как хронический тонзиллит, синусит или зубной кариес."
"Перикардит \n
Профилактика:
 Способы лечения напрямую зависят от тяжести течения болезни: при острой форме показана госпитализация с 
 целью исключения тампонады, при более лёгком протекании возможно нахождении в амбулаторных условиях.
 Для терапевтического эффекта применяют нестероидные противовоспалительные 
 средства (НПВС). Чаще используют ибупрофен ввиду редких побочных проявлений, 
 благотворного влияния на коронарный кровоток и широкий диапазон терапевтических доз."
"Аритмия сердца \n
Профилактика:
 Когда точно определена разновидность аритмии, назначают вторичную профилактику. 
 Вторичную профилактику не проводят при брадикардии.
 При тахикардиях применяют некоторые антиаритмические лекарственные средства: 
 Антагонисты кальция (Дилтиазем, Верапамил); Соталекс; Адреноблокаторы 
 (Эгилок, Анаприлин, Конкор, Атенолол); Кардарон; Пропанорм; Аллапинин и другие."
"фибрилляция \n
Профилактика:
 Для лечения больных с фибрилляцией предсердий используются две принципиальные стратегии:
 rhythm control – восстановление синусового ритма (медикаментозная или электрическая кардиоверсия с последующей профилактикой рецидива;
 rate control – контроль ЧЖС, сочетаемый с антикоагулянтной или антиагрегантной терапией (если ФП сохраняется)."
"Экстрасистолия \n
Профилактика:
 Экстрасистолия, возникшая на фоне нейрогенных факторов, требует дополнительной консультации невролога. 
 Помимо этого назначаются специальные успокоительные сборы либо препараты седативного действия. 
 Возникшая в качестве побочного эффекта от приема лекарств экстрасистолия требует немедленной их отмены."
"Гипертония \n
Профилактика:
 Следует принять положение полусидя в постели или в удобном кресле;
 согреть стопы и голени с помощью грелки, ножной горячей ванны, горчичников на голени;
 внутрь принять корвалол (или валокордин) — 30-35 капель, а также внеочередную дозу того препарата, который систематически принимает больной;
 появление загрудинной боли требует немедленного приема нитроглицерина под язык;
 необходимо воздержаться от еды;
 при интенсивной головной боли можно принять таблетку мочегонного препарата, если он уже применялся для лечения."
"Гипотензия \n
Профилактика:
 — рациональное чередование рабочего дня с отдыхом;
 — здоровый сон;
 — правильное питание;
 — исключение вредных привычек;
 — умеренные физические нагрузки (лечебная физкультура);
 — прогулки на свежем воздухе;
 — закаливание организма (контрастный душ)."
"Ишемическая болезнь сердца \n
Профилактика:
 При консервативном лечении ИБС, как правило, используют следующие группы препаратов: 
 бета-адреноблокаторы (бисопролол, пропранолол, атенолол, небиволол)"
"Стенокардия \n
Профилактика:
 Каждый человек знает, что лучшее лечение болезни – ее профилактика. Чтобы быть всегда в хорошей форме,
 и не хвататься за сердце при малейшем увеличении нагрузки, надо:
 Следить за своим весом, старясь не допустить ожирения;
 Навсегда забыть о курении и прочих вредных привычках;
 Своевременно лечить сопутствующие заболевания, которые могут стать предпосылкой к развитию стенокардии;
 При генетической предрасположенности к сердечным заболеваниям, больше уделять времени укреплению сердечной мышцы и повышению эластичности сосудов, посещая кабинет лечебной физкультуры и строго следуя всем советам лечащего врача;
 Вести активный образ жизни, ведь гиподинамия – один из факторов риска в развитии стенокардии и прочих заболеваний сердца и сосудов."
"Кардиосклероз \n
Профилактика:
 Еду следует принимать малыми порциями. Соль разрешается в очень малых количествах (она увеличивает отеки).
 Мочегонно действуют творог с сахаром (до 1 кг в день) и дни яблочной диеты (1 кг сырых некислых яблок)
 или же сочетание творога с яблоками 1:1."
"Коронарные заболевание сердца \n
Профилактика:
 - Отказ от курения
 - Коррекция веса тела при ожирении
 - Регулярно двигаться
 - Постоянно соблюдать все терапевтические мероприятия, направленные на коррекцию артериального давления, содержания холестерина и сахара в крови.
 - Необходимо сбалансировать трудовую жизнь, при этом минимизировать факторы риска."
"Атеросклероз \n
Профилактика:
 Атеросклероз лечат, используя медикаменты: специальные препараты статины, 
 которые снижают концентрацию холестерина в крови. Назначается диета. 
 Также используют сосудорасширяющие препараты. Это правильная тактика, 
 которая позволяет улучшить кровоток. На поздних стадиях развития атеросклероза,
 когда через суженный сосуд кровь постепенно перестает поступать в орган,
 назначают хирургическое лечение по замене сосуда или его расширению (стентирование)."
"Сердечная астма \n
Профилактика:
 При сердечной астме с сильной одышкой и болевым синдромом применяются наркотические анальгетики.
 В случае угнетения дыхания, бронхоспазма, хронического легочного сердца, отека мозга, 
 они могут быть заменены нейролептаналгетиком - дроперидолом."
"Сердечная недостаточность \n
Профилактика:
 Для предотвращения сердечной недостаточности необходимо правильное питание, 
  достаточная физическая активность, отказ от вредных привычек. 
  Все заболевания сердечнососудистой системы должны быть своевременно выявлены и пролечены."
"Митральный стеноз \n
Профилактика:
 Медикаментозная терапия при митральном стенозе необходима 
 с целью профилактики инфекционного эндокардита (антибиотики), 
 уменьшения выраженности сердечной недостаточности 
 (сердечные гликозиды, диуретики), купирования аритмий (бета-блокаторы). 
 При тромбоэмболиях в анамнезе назначается подкожное введение гепарина под контролем МНО, прием антиагрегантов."
"Недостаточность митрального клапана \n
Профилактика:
 Профилактика состоит в предупреждении первичных заболеваний,
 которые приводят к развитию недостаточности митрального клапана, а также своевременном лечении."
"Пролапс митрального клапана \n
Профилактика:
 Пролапс митрального клапана без нарушения его функции (без обратного потока крови) не требует лечения,
 но требует наблюдения то есть проведение Эхокг контроля один раз в два года для своевременного выявления возможных осложнений."
"Аортальный стеноз \n
Профилактика:
 Все пациенты, в т.ч. с бессимптомным, полностью компенсированным аортальным стенозом,
 должны находиться под тщательным наблюдением кардиолога.
 Им рекомендуются проведение ЭхоКГ каждые 6-12 месяцев.
 Данному контингенту больных с целью профилактики инфекционного эндокардита необходим превентивный
 прием антибиотиков перед стоматологическими (лечение кариеса, удаление зубов и т. д.) и другими инвазивными процедурами."
"Недостаточность аортального клапана \n
Профилактика:
 При заболевании 1 и 2 степени тяжести лечение, как правило, не проводится.
 Назначается лишь наблюдение и плановое обследование.
 Лечение при 3 и 4 степени тяжести определяется формой заболевания,
 симптомами и первичной причиной. Медикаменты назначаются с учетом проводимого основного лечения."
"Порок сердца \n
Профилактика:
 Консервативное лечение направлено на профилактику рецидивов и осложнений основного заболевания,
 коррекцию нарушения ритма сердца, профилактику и лечение сердечной недостаточности."
"Всего лишь простуда!) Горячий чай и мед в помощь."


                          ))

(define (болезнь res)
  (let ([max (array-ref res 0)] [index 0])
    (do ((i 0 (1+ i)) )
      ((= i (array-end res 0)) (format "Ваш диагноз: ~A \n [Вероятность диагноза - ~A]\n Выздоравливайте!\n" (list-ref список-болезней index) max ))
      (when (< max (array-ref res i))
        (set! max (array-ref res i))
        (set! index i)
        )
      )
    )
  )

(define (compute)
  (if (= 0 NUMHID)
      (display "Network wasn't loaded or created")
      (let ( (in (make-vector NUMIN)) (res 0) )
        ;(display "Input array:")
        ;(readvector in)
        (readdata in)
        (set! res (getoutput in))
        (newline)
        ;(display " Полученный выход: ")
        ;(print res)
        ;(print-vector res)
        (display (болезнь res))
        (newline)
        )
      )
  )

(define (menu)
  (display "Для начала работы выберите:")
  (newline)
  (display "1.Обучение сети")
  (newline)
  (display "2.Определение диагноза по ссимптомам")
  (newline)
  (display "3.Завершение работы")
  (newline)
  (define choice (read))
  (cond
    ((= choice 1) (main))
    ((= choice 2) (read-network "network.txt") (compute) (menu))
    ((= choice 3) )
    (else (menu))
    )
  )

  (menu)