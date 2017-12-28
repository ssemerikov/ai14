(*трехлойная нейронная сеть (вход, скрытый слой, выход*)

val NUMIN=ref 0   (*размерность входа*)
and NUMHID=ref 0  (*размерность скрытого слоя*)
and NUMOUT=ref 0  (*размерность выхода*)

(*матрицы весовых коэффициентов*)
and WeightIH=ref (Array2.array(0,0,0.0)) (*соединения входа и скрытого слоя*)
and WeightHO=ref (Array2.array(0,0,0.0)) (*соединения скрытого слоя и выходa*)

(*для нормализации*)
and mini=ref 0.0
and maxi=ref 0.0
and mino=ref 0.0
and maxo=ref 0.0
and GlobalMinError=ref 100000000.0

exception SizeError of string

fun random()=
  Random.random(Random.newgen())

(*функции для создания векторов и матриц*)

fun makevector(size, value)=
  if size>0 then
      Array.array(size,value)
  else
      raise SizeError "Недозволенная размерность вектора"

fun makematrix(m,n,value)=
  if m>0 andalso n>0 then
      Array2.array(m, n, value)
  else
      raise SizeError "Недозволенная размерность матрицы"

(*функции для получения/установки значений векторов и матриц*)

fun getvvalue(obj, i)=
  Array.sub(obj,i)

fun getmvalue(obj,i,j)=
  Array2.sub(obj,i,j)

fun setvvalue(vec,i,value)=
  Array.update(vec,i,value)

fun setmvalue(matrix,i,j,value)=
  Array2.update(matrix,i,j,value)

fun vectorfrommatrix(m,index)=
let 
  val res=makevector(Array2.nCols(m),0.0)
  and i=ref 0
in
  while !i < Array2.nCols(m) do
  (
    setvvalue(res,!i,getmvalue(m, index, !i));
    i := !i + 1
  );
  res
end

fun printvector(v)=
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
  
  WeightIH := makematrix(1 + !NUMIN, !NUMHID, 0.0);
  WeightHO := makematrix(1 + !NUMHID, !NUMOUT, 0.0);

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

fun read_int stream =
  Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream)

fun read_real stream =
  Option.valOf (TextIO.scanStream (Real.scan) stream)

fun readnetwork(filename)=
let
  val f=TextIO.openIn(filename)
  and i=ref 0
  and k=ref 0
in
  NUMIN := read_int f;
  NUMOUT := read_int f;
  NUMHID := read_int f;
  makenetwork(!NUMIN,!NUMOUT,!NUMHID);
  i := 0;
  while !i < !NUMIN+1 do
  (   
    k := 0;
    while !k < !NUMHID do
    (
      setmvalue(!WeightIH,!i,!k, read_real f);
      k := !k + 1
    );
    i := !i + 1
  );
  i := 0;
  while !i < !NUMHID+1 do
  (   
    k := 0;
    while !k < !NUMOUT do
    (
      setmvalue(!WeightHO,!i,!k, read_real f);
      k := !k + 1
    );
    i := !i + 1
  );
  mini := read_real f;
  maxi := read_real f;
  mino := read_real f;
  maxo := read_real f;
  GlobalMinError := read_real f;
  TextIO.closeIn(f)
end

fun writenetwork(filename)=
let
  val f=TextIO.openOut(filename)
  and i=ref 0
  and k=ref 0
in
  TextIO.output(f,Int.toString(!NUMIN));
  TextIO.output(f," ");
  TextIO.output(f,Int.toString(!NUMOUT));
  TextIO.output(f," ");
  TextIO.output(f,Int.toString(!NUMHID));
  TextIO.output(f,"\n");
  i := 0;
  while !i < !NUMIN+1 do
  (   
    k := 0;
    while !k < !NUMHID do
    (
      TextIO.output(f,Real.toString(getmvalue(!WeightIH,!i,!k)));
      TextIO.output(f," ");
      k := !k + 1
    );
    TextIO.output(f,"\n");
    i := !i + 1
  );
  i := 0;
  while !i < !NUMHID+1 do
  (   
    k := 0;
    while !k < !NUMOUT do
    (
      TextIO.output(f,Real.toString(getmvalue(!WeightHO,!i,!k)));
      TextIO.output(f," ");
      k := !k + 1
    );
    TextIO.output(f,"\n");
    i := !i + 1
  );
  TextIO.output(f,Real.toString(!mini));
  TextIO.output(f," ");
  TextIO.output(f,Real.toString(!maxi));
  TextIO.output(f," ");
  TextIO.output(f,Real.toString(!mino));
  TextIO.output(f," ");
  TextIO.output(f,Real.toString(!maxo));
  TextIO.output(f," ");
  TextIO.output(f,Real.toString(!GlobalMinError));
  TextIO.output(f,"\n");
  TextIO.closeOut(f)
end    

(*обучение нейронной сети*)
fun train(TrainInput,TrainTarget,Err,MaxCount,DoOut,NetworkFile)=
let
  val NUMPAT=Array2.nRows(TrainInput) (*число обучающих шаблонов*)
in
let
  val Error=ref (Err+1.0)
  and eta=0.5
  and alpha=0.9
  and ranpat=makevector(NUMPAT,0)
  and NumPattern=NUMPAT
  and NumInput=(!NUMIN)
  and NumHidden=(!NUMHID)
  and NumOutput=(!NUMOUT)
         (*временные массивы*)
  and DeltaWeightIH=makematrix(1+ !NUMIN,!NUMHID, 0.0)
  and DeltaWeightHO=makematrix(1+ !NUMHID,!NUMOUT, 0.0)
  and SumDOW=makevector(!NUMHID,0.0)
  and DeltaH=makevector(!NUMHID,0.0)
  and DeltaO=makevector(!NUMOUT,0.0)
  and SumH=makevector(!NUMHID,0.0)
  and Hidden=makevector(!NUMHID,0.0)
  and SumO=makevector(!NUMOUT,0.0)
  and Output=makevector(!NUMOUT,0.0)
  and Input=makematrix(NUMPAT,!NUMIN, 0.0)
  and Target=makematrix(NUMPAT,!NUMOUT, 0.0)
  and i=ref 0
  and k=ref 0
  and epoch=ref 0
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

    (*цикл обучения по достижению заданной ошибки или числа итераций*)
    epoch := 0;
    while !epoch < MaxCount andalso !Error<Err do
    let
      val p=ref 0
      and gen=Random.newgen()
      and np=ref 0
      and j=ref 0
    in
      (*перемешиваем шаблоны*)
      while !p<NumPattern do
      (
        setvvalue(ranpat,!p,Random.range(0,NumPattern) gen); 
        p := !p + 1
      );
      Error := 0.0;
      (*цикл обучения по шаблонам*)
      np := 0;
      while !np<NumPattern do
      (
        (*выбираем шаблон*)
        p := getvvalue(ranpat,!np);
        (*активация скрытого слоя*)
        j := 0;
        while !j < NumHidden do
        ( 
          setvvalue(SumH,!j,getmvalue(!WeightIH,0,!j));
          i := 0;
          while !i < NumInput do
          (
            setvvalue(SumH,!j, getvvalue(SumH,!j)+getmvalue(Input,!p,!i)*getmvalue(!WeightIH, 1+ !i, !j));
            i := !i + 1
          );
          setvvalue(Hidden,!j,1.0/(1.0+Math.exp(~(getvvalue(SumH,!j)))));
          j := !j + 1
        );
        (*активация выходного слоя и вычисление ошибки*)
        k := 0;
        while !k < NumOutput do
        (
          setvvalue(SumO,!k,getmvalue(!WeightHO,0,!k));
          j := 0;
          while !j < NumHidden do
          ( 
            setvvalue(SumO, !k, getvvalue(SumO,!k)+getvvalue(Hidden,!j)*getmvalue(!WeightHO, 1+ !j, !k));
            j := !j + 1
          );
          setvvalue(Output,!k, 1.0/(1.0+Math.exp(~(getvvalue(SumO,!k)))));
          Error := !Error + 0.5*(getmvalue(Target,!p, !k)-getvvalue(Output,!k))*
                        (getmvalue(Target,!p, !k)-getvvalue(Output,!k));
          setvvalue(DeltaO,!k,(getmvalue(Target,!p, !k)- getvvalue(Output,!k))*
                      getvvalue(Output,!k)*(1.0-getvvalue(Output,!k)));
          k := !k + 1
        );
        (*обратное распространение ошибки на скрытый слой*)
        j := 0;
        while !j < NumHidden do
        ( 
          setvvalue(SumDOW,!j, 0.0);
          k := 0;
          while !k < NumOutput do
          (
            setvvalue(SumDOW,!j,getvvalue(SumDOW,!j)+getmvalue(!WeightHO,1+ !j, !k)*getvvalue(DeltaO,!k));
            k := !k + 1
          );
          setvvalue(DeltaH,!j,getvvalue(SumDOW, !j)*getvvalue(Hidden, !j)*(1.0 -getvvalue(Hidden,!j)));
          j := !j + 1
        );
        j := 0;
        while !j < NumHidden do
        ( 
          setmvalue(DeltaWeightIH,0, !j,eta*getvvalue(DeltaH,!j)+alpha*getmvalue(DeltaWeightIH,0,!j));
          setmvalue(!WeightIH,0, !j, getmvalue(!WeightIH,0, !j)+getmvalue(DeltaWeightIH,0,!j));
          i := 0;
          while !i < NumInput do
          (
            setmvalue(DeltaWeightIH, 1+ !i, !j,
                eta*getmvalue(Input,!p,!i)*getvvalue(DeltaH,!j)+alpha*getmvalue(DeltaWeightIH, 1+ !i, !j));
            setmvalue(!WeightIH, 1+ !i, !j,getmvalue(!WeightIH, 1+ !i, !j)+getmvalue(DeltaWeightIH,1+ !i, !j));
            i := !i + 1
          );
          j := !j + 1
        );
        k := 0;
        while !k < NumOutput do
        (
          setmvalue(DeltaWeightHO,0,!k,eta*getvvalue(DeltaO,!k)+alpha*getmvalue(DeltaWeightHO,0,!k));
          setmvalue(!WeightHO,0, !k,getmvalue(!WeightHO,0, !k)+getmvalue(DeltaWeightHO, 0, !k));
          j := 0;
          while !j < NumHidden do
          ( 
            setmvalue(DeltaWeightHO,1+ !j, !k,
                      eta*getvvalue(Hidden,!j)*getvvalue(DeltaO,!k)+alpha*getmvalue(DeltaWeightHO,1+ !j, !k));
            setmvalue(!WeightHO, 1+ !j, !k,getmvalue(!WeightHO, 1+ !j, !k)+getmvalue(DeltaWeightHO, 1+ !j, !k));
            j := !j + 1
          );
          k := !k + 1
        );
        
      if DoOut andalso Int.rem(!epoch, 10) = 0 then (*отладочный вывод*)
        print ("epoch=" ^ Int.toString(!epoch) ^ ", error=" ^ Real.toString(!Error) ^ "\n")
      else
      ();
      if !Error < !GlobalMinError then
      (
        GlobalMinError := !Error;
        print ("epoch=" ^ Int.toString(!epoch) ^ ", (min)error=" ^ Real.toString(!Error) ^ "\n");
        writenetwork(NetworkFile)
      )
      else
      ();
      np := !np + 1
    );
    epoch := !epoch + 1
    end;
  writenetwork(NetworkFile);
  Error
end
end

(*подача сигнала на вход сети и получение результата*)
fun getoutput(BeInput)=
let
  val Input=makevector(!NUMIN,0.0)
  and Output=makevector(!NUMOUT,0.0)
  and result=makevector(!NUMOUT,0.0)
  and SumH=makevector(!NUMHID,0.0)
  and Hidden=makevector(!NUMHID,0.0)
  and SumO=makevector(!NUMOUT,0.0)
  and NumInput=(!NUMIN)
  and NumHidden=(!NUMHID)
  and NumOutput=(!NUMOUT)
  and k=ref 0
  and i=ref 0
  and j=ref 0
in
    (*нормализация входа*)
    k := 0;
    while !k < NumInput do
    (
      setvvalue(Input,!k, (getvvalue(BeInput,!k)- !mini)/(!maxi - !mini));
      k := !k +1
    );
    
    (*активация скрытого слоя*)
    j := 0;
    while !j < NumHidden do
    (
      setvvalue(SumH,!j,getmvalue(!WeightIH,0,!j));
      i := 0;
      while !i < NumInput do
      ( 
        setvvalue(SumH, !j, getvvalue(SumH, !j)+getvvalue(Input,!i)*getmvalue(!WeightIH,1+ !i, !j));
        i := !i + 1
      );
      setvvalue(Hidden,!j,1.0/(1.0+Math.exp(~(getvvalue(SumH, !j)))));
      j := !j + 1
    );
    
    (*активация выходного слоя*)
    k := 0;
    while !k < NumOutput do
    (
      setvvalue(SumO,!k,getmvalue(!WeightHO,0,!k));
      j := 0;
      while !j < NumHidden do
      (
        setvvalue(SumO,!k, getvvalue(SumO,!k)+getvvalue(Hidden,!j)*getmvalue(!WeightHO,1+ !j, !k));
        j := !j + 1
      );
      setvvalue(Output,!k,1.0/(1.0+Math.exp(~(getvvalue(SumO, !k)))));
      k := !k +1
    );
    
    (*денормализация выхода*)
    k := 0;
    while !k < NumOutput do
    (
      setvvalue(result,!k, getvvalue(Output,!k)*(!maxo - !mino)+ !mino);
      k := !k +1
    );
    result
end

(*пример создания использования нейронной сети*)

fun main()=
let 
  val NUMPAT=ref 60 (*число обучающих шаблонов - может переопределяться в файле*)
  and f=TextIO.openIn("etalons.txt")
in 
  (*форматы файлов матриц: число_строк число_столбцов данные*)
  NUMPAT := read_int f;
  NUMIN := read_int f;
  NUMOUT := read_int f;
  
  NUMHID := !NUMIN * 2 + 1; (*число нейронов в скрытом слое*)

  let 
    val Input=makematrix(!NUMPAT,!NUMIN,0.0)
    and Output=makematrix(!NUMPAT,!NUMOUT,0.0)
    and i=ref 0
    and k=ref 0
    and inp=makevector(!NUMIN,0.0)
    and res=ref (makevector(!NUMOUT,0.0))
    and out=makevector(!NUMOUT,0.0)
  in
  i :=0;
  while !i < !NUMPAT do
  (
    k := 0;
    while !k < !NUMIN do
    (
      setmvalue(Input,!i, !k, read_real f);
      k := !k + 1
    );
    k := 0;
    while !k < !NUMOUT do
    (
      setmvalue(Output,!i, !k, read_real f);
      k := !k + 1
    );
    i := !i + 1
  );
  
  TextIO.closeIn(f);

  makenetwork(!NUMIN,!NUMOUT,!NUMHID);
  print("Размерность входа - " ^ Int.toString(!NUMIN) ^ ", размерность выхода - " ^  Int.toString(!NUMOUT) ^ 
        ", число шаблонов - " ^ Int.toString(!NUMPAT)^ "\n");
  train(Input,Output,0.00001,150000, true, "network.txt");
  (* readnetwork("network.txt"); *)
  
  print "Исходные данные:\n";
  i := 0;
  while !i < !NUMPAT do
  (
    print "Вход: ";
    printvector (vectorfrommatrix(Input,!i));
    res := getoutput(vectorfrommatrix(Input, !i));
    print " Эталонный выход: ";
    printvector (vectorfrommatrix(Output, !i));
    print " Полученный выход: ";
    printvector(!res);
    if Int.rem(!i,10)=0 then
    (
     print "Press Enter to continue";
     TextIO.input1(TextIO.stdIn);
     ()
    )
    else
    ();
    i := !i + 1
  )
  end;
  print "Тестовые данные:\n";
  let
    val f=TextIO.openIn("test.txt")
    and count=ref 0
    and i=ref 0
    and k=ref 0
    and inp=makevector(!NUMIN,0.0)
    and res=ref (makevector(!NUMOUT,0.0))
  in
    count := read_int f;
    NUMIN := read_int f;

    i := 0;
    while !i < !count do
    (
      k := 0;
      while !k < !NUMIN do
      (
        setvvalue(inp, !k, read_real f);
        k := !k + 1
      );
      print "Вход: ";
      printvector inp;
      res := getoutput(inp);
      print " Полученный выход: ";
      printvector(!res);
      if Int.rem(!i,10)=0 then
      (
       print "Press Enter to continue";
       TextIO.input1(TextIO.stdIn);
       ()
      )
      else
      ();
      i := !i + 1
    );
    TextIO.closeIn(f)
  end
end

val _=main()
