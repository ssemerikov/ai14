(*трехлойная нейронная сеть (вход, скрытый слой, выход*)

val NUMIN=ref 0   (*размерность входа*)
and NUMHID=ref 0  (*размерность скрытого слоя*)
and NUMOUT=ref 0 (*размерность выхода*)

(*матрицы весовых коэффициентов*)
and WeightIH=ref (Array2.array(0,0,0.0)) (*соединения входа и скрытого слоя*)
and WeightHO=ref (Array2.array(0,0,0.0)) (*соединения скрытого слоя и выходa*)

(*для нормализации*)
and mini=ref 0.0
and maxi=ref 0.0
and mino=ref 0.0
and maxo=ref 0.0
and GlobalMinError=ref 100000000

exception SizeError of string

fun random()=
  Random.random(Random.newgen())

(*функции для создания векторов и матриц*)

fun makevector(size)=
  if size>0 then
      Array.array(size,0.0)
  else
      raise SizeError "Недозволенная размерность вектора"

fun makematrix(m,n)=
  if m>0 andalso n>0 then
      Array2.array(m, n, 0.0)
  else
      raise SizeError "Недозволенная размерность матрицы"


fun vectorfrommatrix(m,index)=
  Array2.row(m,index)

(*функции для получения/установки значений векторов и матриц*)

fun getvvalue(obj, i)=
  Array.sub(obj,i)

fun getmvalue(obj,i,j)=
  Array2.sub(obj,i,j)

fun setvvalue(vec,i,value)=
  Array.update(vec,i,value)

fun setmvalue(matrix,i,j,value)=
  Array2.update(matrix,i,j,value)

fun printvector(v:real array):unit=
  let
    val i=ref 0
  in
    while !i < Array.length(v) do
    (
      print ((Real.toString (getvvalue(v,!i)))^" ");
      i := !i + 1
    );
    print "\n"
  end 

(*создание нейронной сети*)

fun makenetwork (NUMIN0, NUMOUT0, NUMHID0)=
let
  val smallwt=0.5
  and j=ref 0
  and i=ref 0
  and k=ref 0
in
  NUMIN := NUMIN0;
  NUMOUT := NUMOUT0;
  NUMHID := NUMHID0;
  
  WeightIH := makematrix(1 + !NUMIN, !NUMHID);
  WeightHO := makematrix(1 + !NUMHID, !NUMOUT);

  (*устанавливаем случайные весовые коэффициенты*)
  j := 0;
  while !j < !NUMHID do
  (
    i := 0;
    while !i < !NUMIN+1 do
    (
      setmvalue(!WeightIH,!i,!j, 2.0*(random()-0.5)*smallwt);
      i := !i + 1
    );
    j := !j + 1
  );

  k := 0;
  while !k < !NUMOUT do
  (
    j := 0;
  
    while !j < !NUMHID+1 do
    (
      setmvalue(!WeightHO,!j,!k, 2.0*(random()-0.5)*smallwt);
      j := !j + 1
    );
    k := !k + 1
  )
end

(*с помощью этих функций можно сохранять и 
  загружать матрицы весовых коэффициентов*)

fun readnetwork(filename)=
let
  val f=TextIO.openIn(filename)
in
(*
  (set! NUMIN (read f))
  (set! NUMOUT (read f))
  (set! NUMHID (read f))
 *)
  makenetwork(!NUMIN,!NUMOUT,!NUMHID);
   (*
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
   *)
  TextIO.closeIn(f)
end

val _=readnetwork("network.txt")
     (*
fun writenetwork(filename)=
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
  )    *)


(*обучение нейронной сети*)
fun train(TrainInput,TrainTarget,Err,MaxCount,DoOut,NetworkFile)=
let
  val NUMPAT=Array2.nRows(TrainInput) (*число обучающих шаблонов*)
in
let
  val Error=Err+1.0
  and eta=0.5
  and alpha=0.9
  and ranpat=makevector(NUMPAT)
  and NumPattern=NUMPAT
  and NumInput=(!NUMIN)
  and NumHidden=(!NUMHID)
  and NumOutput=(!NUMOUT)
         (*временные массивы*)
  and DeltaWeightIH=makematrix(1+ !NUMIN,!NUMHID)
  and DeltaWeightHO=makematrix(1+ !NUMHID,!NUMOUT)
  and SumDOW=makevector(!NUMHID)
  and DeltaH=makevector(!NUMHID)
  and DeltaO=makevector(!NUMOUT)
  and SumH=makevector(!NUMHID)
  and Hidden=makevector(!NUMHID)
  and SumO=makevector(!NUMOUT)
  and Output=makevector(!NUMOUT)
  and Input=makematrix(NUMPAT,!NUMIN)
  and Target=makematrix(NUMPAT,!NUMOUT)
  and i=ref 0
  and k=ref 0
in
    
    (*копируем тренировочные матрицы во временные во избежание порчи*)
  i := 0;
  while !i < NUMPAT do
  (
    k := 0;
    while !k < !NUMIN do
    (
      setmvalue(Input,!i,!k,getmvalue(TrainInput,!i,!k));
      k := !k + 1
    );
    i := !i + 1
  );

  i := 0;
  while !i < NUMPAT do
  (
    k := 0;
    while !k < !NUMOUT do
    (
      setmvalue(Target,!i,!k,getmvalue(TrainTarget,!i,!k));
      k := !k + 1
    );
    i := !i + 1
  );
    (*если существует файл NetworkFile - загрузим его для дообучения*)

    if FileSys.access(NetworkFile,[]) then 
      readnetwork(NetworkFile)
    else
    (
       mini := getmvalue(Input,0,0);
       maxi := getmvalue(Input,0,0);
       mino := getmvalue(Target,0,0);
       maxo := getmvalue(Target,0,0);
    
       (*поиск граничных значений в числовых массивах*)
       i := 0;
       while !i < NumPattern do
       (
         k := 0;
         while !k < NumInput do
         (
           if !mini > getmvalue(Input,!i,!k) then
             mini := getmvalue(Input,!i,!k)
           else
             ();
           if !maxi < getmvalue(Input,!i,!k) then
             maxi := getmvalue(Input,!i,!k)
           else
             ();
           k := !k + 1
         );
         k := 0;
         while !k < NumOutput do
         (
           if !mino > getmvalue(Target,!i,!k) then
             mino := getmvalue(Target,!i,!k)
           else
             ();
           if !maxo < getmvalue(Target,!i,!k) then
             maxo := getmvalue(Target,!i,!k)
           else
             ();
           k := !k + 1
         );
         i := !i + 1
       )
    );
    
    (*нормализация*)
    i := 0;       
    while !i < NumPattern do
    (         
      k := 0; 
      while !k < NumInput do
      (
        setmvalue(Input,!i,!k, (getmvalue(Input,!i,!k) - !mini) / (!maxi - !mini) );
        k := !k + 1
      );
      k := 0;
      while !k < NumOutput do
      (
        setmvalue(Target,!i,!k, (getmvalue(Target,!i,!k) - !mino) / (!maxo - !mino));
        k := !k + 1
      );       
      i := !i + 1
    );              

      (*
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
                          ( *  
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
                               ( * 
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
                       ( *
                        0.5 
                        (- (getmvalue Target p k) (getvvalue Output k))
                        (- (getmvalue Target p k) (getvvalue Output k))
                        )
                       )
                )
          (setvvalue DeltaO k 
                     ( *
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
                        ( *
                         (getmvalue WeightHO (1+ j) k)
                         (getvvalue DeltaO k)
                         )
                        )
                       )
            )
          (setvvalue DeltaH j 
                     ( * 
                      (getvvalue SumDOW j)
                      (getvvalue Hidden j)
                      (- 1.0 (getvvalue Hidden j))
                      )
                     )
          )
        (do [(j 0 (+ 1 j))] ((= j NumHidden))            
          (setmvalue DeltaWeightIH 0 j 
                     (+
                      ( * 
                       eta 
                       (getvvalue DeltaH j)
                       )
                      ( *
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
                        ( * 
                         eta 
                         (getmvalue Input p i)
                         (getvvalue DeltaH j)
                         )
                        ( *
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
                      ( * 
                       eta 
                       (getvvalue DeltaO k)
                       )
                      ( *
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
                        ( * 
                         eta 
                         (getvvalue Hidden j)
                         (getvvalue DeltaO k)
                         )
                        ( *
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
*)
  ()
end
end

(*
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
                      ( * 
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
                           ( * 
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
                           ( *
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

  (set! NUMHID (+ ( * NUMIN 2) 1)) ;число нейронов в скрытом слое
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
  ;(read-network "network.txt")
  
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



val _=main()
*)