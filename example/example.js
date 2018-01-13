/*трехлойная нейронная сеть (вход, скрытый слой, выход*/

var NUMIN=0;   /*размерность входа*/
var NUMHID=0;  /*размерность скрытого слоя*/
var NUMOUT=0;  /*размерность выхода*/

/*матрицы весовых коэффициентов*/
var WeightIH=[]; /*соединения входа и скрытого слоя*/
var WeightHO=[]; /*соединения скрытого слоя и выходa*/

/*для нормализации*/
var mini=0.0;
var maxi=0.0;
var mino=0.0;
var maxo=0.0;
var GlobalMinError=100000000.0;

/*exception SizeError of string*/

function random()
{
  return Math.random();
}

/*функции для создания векторов и матриц*/

function makevector(size, value)
{
  if(size>0)
  {
    var arr=[];
    for(var i=0;i<size;i++)
      arr[i]=value;
    return arr;
  }
  else
    throw "Недозволенная размерность вектора";
}

function makematrix(m,n,value)
{
  if(m>0 && n>0)
  {
    var arr=[];
    for(var i=0;i<m;i++)
      arr[i]=[];
    for(var i=0;i<m;i++)
      for(var j=0;j<n;j++)
        arr[i][j]=0;
    return arr;
  }
  else
    throw "Недозволенная размерность матрицы";
}

function vectorfrommatrix(m,index)
{
  var res=makevector(m[0].length,0.0);

  for(var i=0;i<m[0].length;i++)
    res[i]=m[index][i];
  return res;
}

function printvector(v)
{
  for(var i=0;i<v.length;i++)
    process.stdout.write(v[i]+" ");
  console.log("");
}

/*создание нейронной сети*/

function makenetwork (NUMIN0, NUMOUT0, NUMHID0)
{
  var smallwt=0.5;

  NUMIN = NUMIN0;
  NUMOUT = NUMOUT0;
  NUMHID = NUMHID0;
  
  WeightIH = makematrix(1 + NUMIN, NUMHID, 0.0);
  WeightHO = makematrix(1 + NUMHID, NUMOUT, 0.0);

  /*устанавливаем случайные весовые коэффициенты*/
  for(var j=0;j < NUMHID;j++)
  {
    for(var i=0;i < NUMIN+1;i++)
      WeightIH[i][j]=2.0*(random()-0.5)*smallwt;
  }

  for(var k=0;k < NUMOUT;k++)
  {
    for(var j=0;j < NUMHID+1;j++)
      WeightHO[j][k]=2.0*(random()-0.5)*smallwt;
  }
}


/*с помощью этих функций можно сохранять и 
  загружать матрицы весовых коэффициентов*/

function readnetwork(filename)
{
  var f=require('fs').createReadStream(filename);

/*
  NUMIN := read_int f;
  NUMOUT := read_int f;
  NUMHID := read_int f;
  makenetwork(NUMIN,NUMOUT,NUMHID);
  i := 0;
  while i < NUMIN+1 do
  (   
    k := 0;
    while k < NUMHID do
    (
      WeightIH[i][k]=read_real f;
      k := k + 1
    );
    i := i + 1
  );
  i := 0;
  while i < NUMHID+1 do
  (   
    k := 0;
    while k < NUMOUT do
    (
      WeightHO[i][k]=read_real f;
      k := k + 1
    );
    i := i + 1
  );
  mini := read_real f;
  maxi := read_real f;
  mino := read_real f;
  maxo := read_real f;
  GlobalMinError := read_real f;
  TextIO.closeIn(f)
*/
}

function writenetwork(filename)
{
  var f=require('fs').createWriteStream(filename);

  f.write(NUMIN+" "+NUMOUT+" "+NUMHID+"\n")
  for(var i=0;i < NUMIN+1;i++)
  {   
    for(var k=0;k < NUMHID;k++)
      f.write(WeightIH[i][k]+" ");
    f.write("\n");
  }
  for(var i=0;i < NUMHID+1;i++)
  {
    for(var k=0;k < NUMOUT;k++)
    {
      f.write(WeightHO[i][k]+" ");
    }
    f.write("\n");
  }
  f.write(mini+" "+maxi+" "+mino+" "+maxo+" "+GlobalMinError+"\n");
  f.end();
}


/*
var m=makenetwork(3,4,5);
readnetwork("network.txt");
writenetwork("test.txt");
*/

/*



/*обучение нейронной сети*)
fun train(TrainInput,TrainTarget,Err,MaxCount,DoOut,NetworkFile)=
let
  val NUMPAT=Array2.nRows(TrainInput) /*число обучающих шаблонов*)
in
let
  val Error=ref (Err+1.0)
  and eta=0.5
  and alpha=0.9
  and ranpat=makevector(NUMPAT,0)
  and NumPattern=NUMPAT
  and NumInput=(NUMIN)
  and NumHidden=(NUMHID)
  and NumOutput=(NUMOUT)
         /*временные массивы*)
  and DeltaWeightIH=makematrix(1+ NUMIN,NUMHID, 0.0)
  and DeltaWeightHO=makematrix(1+ NUMHID,NUMOUT, 0.0)
  and SumDOW=makevector(NUMHID,0.0)
  and DeltaH=makevector(NUMHID,0.0)
  and DeltaO=makevector(NUMOUT,0.0)
  and SumH=makevector(NUMHID,0.0)
  and Hidden=makevector(NUMHID,0.0)
  and SumO=makevector(NUMOUT,0.0)
  and Output=makevector(NUMOUT,0.0)
  and Input=makematrix(NUMPAT,NUMIN, 0.0)
  and Target=makematrix(NUMPAT,NUMOUT, 0.0)
  and i=ref 0
  and k=ref 0
  and epoch=ref 0
in
    
    /*копируем тренировочные матрицы во временные во избежание порчи*)
  i := 0;
  while i < NUMPAT do
  (
    k := 0;
    while k < NUMIN do
    (
      Input[i][k]=TrainInput[i][k];
      k := k + 1
    );
    i := i + 1
  );

  i := 0;
  while i < NUMPAT do
  (
    k := 0;
    while k < NUMOUT do
    (
      Target[i][k]=TrainTarget[i][k];
      k := k + 1
    );
    i := i + 1
  );
    /*если существует файл NetworkFile - загрузим его для дообучения*)

    if FileSys.access(NetworkFile,[]) then 
      readnetwork(NetworkFile)
    else
    (
       mini := Input[0][0];
       maxi := Input[0][0];
       mino := Target[0][0];
       maxo := Target[0][0];
    
       /*поиск граничных значений в числовых массивах*)
       i := 0;
       while i < NumPattern do
       (
         k := 0;
         while k < NumInput do
         (
           if mini > Input[i][k] then
             mini := Input[i][k]
           else
             ();
           if maxi < Input[i][k] then
             maxi := Input[i][k]
           else
             ();
           k := k + 1
         );
         k := 0;
         while k < NumOutput do
         (
           if mino > Target[i][k] then
             mino := Target[i][k]
           else
             ();
           if maxo < Target[i][k] then
             maxo := Target[i][k]
           else
             ();
           k := k + 1
         );
         i := i + 1
       )
    );
    
    /*нормализация*)
    i := 0;       
    while i < NumPattern do
    (         
      k := 0; 
      while k < NumInput do
      (
        Input[i][k]=(Input[i][k] - mini) / (maxi - mini);
        k := k + 1
      );
      k := 0;
      while k < NumOutput do
      (
        Target[i][k]=(Target[i][k] - mino) / (maxo - mino);
        k := k + 1
      );       
      i := i + 1
    );              

    /*цикл обучения по достижению заданной ошибки или числа итераций*)
    epoch := 0;
    while epoch < MaxCount andalso GlobalMinError >= Err do
    let
      val p=ref 0
      and gen=Random.newgen()
      and np=ref 0
      and j=ref 0
    in
      /*перемешиваем шаблоны*)
      while p<NumPattern do
      (
        ranpat[p]=Random.range(0,NumPattern) gen; 
        p := p + 1
      );
      Error := 0.0;
      /*цикл обучения по шаблонам*)
      np := 0;
      while np<NumPattern do
      (
        /*выбираем шаблон*)
        p := ranpat[np];
        /*активация скрытого слоя*)
        j := 0;
        while j < NumHidden do
        ( 
          SumH[j]=WeightIH[0][j];
          i := 0;
          while i < NumInput do
          (
            SumH[j]+=Input[p][i]*WeightIH[1+i][j];
            i := i + 1
          );
          Hidden[j]=1.0/(1.0+Math.exp(-(SumH[j])));
          j := j + 1
        );
        /*активация выходного слоя и вычисление ошибки*)
        k := 0;
        while k < NumOutput do
        (
          SumO[k]=WeightHO[0][k];
          j := 0;
          while j < NumHidden do
          ( 
            SumO[k]+=Hidden[j]*WeightHO[1+j][k];
            j := j + 1
          );
          Output[k]=1.0/(1.0+Math.exp(~(SumO[k])));
          Error+=0.5*(Target[p][k]-Output[k])*(Target[p][k]-Output[k]);
          DeltaO[k]=(Target[p][k]- Output[k])*Output[k]*(1.0-Output[k]);
          k := k + 1
        );
        /*обратное распространение ошибки на скрытый слой*)
        j := 0;
        while j < NumHidden do
        ( 
          SumDOW[j]=0.0;
          k := 0;
          while k < NumOutput do
          (
            SumDOW[j]+=WeightHO[1+j][k]*DeltaO[k];
            k := k + 1
          );
          DeltaH[j]=SumDOW[j]*Hidden[j]*(1.0 -Hidden[j]);
          j := j + 1
        );
        j := 0;
        while j < NumHidden do
        ( 
          DeltaWeightIH[0][j]=eta*DeltaH[j]+alpha*DeltaWeightIH[0][j];
          WeightIH[0][j]=WeightIH[0][j]+DeltaWeightIH[0][j];
          i := 0;
          while i < NumInput do
          (
            DeltaWeightIH[1+i][j]=eta*Input[p][i]*DeltaH[j]+alpha*DeltaWeightIH[1+i][j];
            WeightIH[1+i][j]=WeightIH[1+i][j]+DeltaWeightIH[1+i][j];
            i := i + 1
          );
          j := j + 1
        );
        k := 0;
        while k < NumOutput do
        (
          DeltaWeightHO[0][k]=eta*DeltaO[k]+alpha*DeltaWeightHO[0][k];
          WeightHO[0][k]=WeightHO[0][k]+DeltaWeightHO[0][k];
          j := 0;
          while j < NumHidden do
          ( 
            DeltaWeightHO[1+j][k]=eta*Hidden[j]*DeltaO[k]+alpha*DeltaWeightHO[1+j][k];
            WeightHO[1+j][k]\WeightHO[1+j][k]+DeltaWeightHO[1+j][k];
            j := j + 1
          );
          k := k + 1
        );
        
      if DoOut andalso Int.rem(epoch, 100) = 0 then /*отладочный вывод*)
        print ("epoch=" ^ Int.toString(epoch) ^ ", error=" ^ Real.toString(Error) ^ "\n")
      else
      ();
      if Error < GlobalMinError then
      (
        GlobalMinError := Error;
        print ("epoch=" ^ Int.toString(epoch) ^ ", (min)error=" ^ Real.toString(Error) ^ "\n");
        writenetwork(NetworkFile)
      )
      else
      ();
      np := np + 1
    );
    epoch := epoch + 1
    end;
  writenetwork(NetworkFile);
  Error
end
end

/*подача сигнала на вход сети и получение результата*)
fun getoutput(BeInput)=
let
  val Input=makevector(NUMIN,0.0)
  and Output=makevector(NUMOUT,0.0)
  and result=makevector(NUMOUT,0.0)
  and SumH=makevector(NUMHID,0.0)
  and Hidden=makevector(NUMHID,0.0)
  and SumO=makevector(NUMOUT,0.0)
  and NumInput=(NUMIN)
  and NumHidden=(NUMHID)
  and NumOutput=(NUMOUT)
  and k=ref 0
  and i=ref 0
  and j=ref 0
in
    /*нормализация входа*)
    k := 0;
    while k < NumInput do
    (
      Input[k]=(BeInput[k]- mini)/(maxi - mini);
      k := k +1
    );
    
    /*активация скрытого слоя*)
    j := 0;
    while j < NumHidden do
    (
      SumH[j]=WeightIH[0][j];
      i := 0;
      while i < NumInput do
      ( 
        SumH[j]+=Input[i]*WeightIH[1+i][j];
        i := i + 1
      );
      Hidden[j]=1.0/(1.0+Math.exp(~(SumH[j])));
      j := j + 1
    );
    
    /*активация выходного слоя*)
    k := 0;
    while k < NumOutput do
    (
      SumO[k]=WeightHO[0][k];
      j := 0;
      while j < NumHidden do
      (
        SumO[k]+=Hidden[j]*WeightHO[1+j][k];
        j := j + 1
      );
      Output[k]=1.0/(1.0+Math.exp(~(SumO[k])));
      k := k +1
    );
    
    /*денормализация выхода*)
    k := 0;
    while k < NumOutput do
    (
      result[k]= Output[k]*(maxo - mino)+ mino);
      k := k +1
    );
    result
end

/*пример создания использования нейронной сети*)

fun main()=
let 
  val NUMPAT=ref 60 /*число обучающих шаблонов - может переопределяться в файле*)
  and f=TextIO.openIn("etalons.txt")
in 
  /*форматы файлов матриц: число_строк число_столбцов данные*)
  NUMPAT := read_int f;
  NUMIN := read_int f;
  NUMOUT := read_int f;
  
  NUMHID := NUMIN * 2 + 1; /*число нейронов в скрытом слое*)

  let 
    val Input=makematrix(NUMPAT,NUMIN,0.0)
    and Output=makematrix(NUMPAT,NUMOUT,0.0)
    and i=ref 0
    and k=ref 0
    and inp=makevector(NUMIN,0.0)
    and res=ref (makevector(NUMOUT,0.0))
    and out=makevector(NUMOUT,0.0)
  in
  i :=0;
  while i < NUMPAT do
  (
    k := 0;
    while k < NUMIN do
    (
      Input[i][k]=read_real f;
      k := k + 1
    );
    k := 0;
    while k < NUMOUT do
    (
      Output[i][k]=read_real f;
      k := k + 1
    );
    i := i + 1
  );
  
  TextIO.closeIn(f);

  makenetwork(NUMIN,NUMOUT,NUMHID);
  print("Размерность входа - " ^ Int.toString(NUMIN) ^ ", размерность выхода - " ^  Int.toString(NUMOUT) ^ 
        ", число шаблонов - " ^ Int.toString(NUMPAT)^ "\n");
  train(Input,Output,0.000001,150000, true, "network.txt");
  /* readnetwork("network.txt"); *)
  
  print "Исходные данные:\n";
  i := 0;

  while i < NUMPAT do
  (
    print "Вход: ";
    printvector (vectorfrommatrix(Input,i));
    res := getoutput(vectorfrommatrix(Input, i));
    print " Эталонный выход: ";
    printvector (vectorfrommatrix(Output, i));
    print " Полученный выход: ";
    printvector(res);
    if Int.rem(i,10)=0 then
    (
     print "Press Enter to continue";
     TextIO.input1(TextIO.stdIn);
     ()
    )
    else
    ();
    i := i + 1
  )
  end;
  print "Тестовые данные:\n";
  let
    val f=TextIO.openIn("test.txt")
    and count=ref 0
    and i=ref 0
    and k=ref 0
    and inp=makevector(NUMIN,0.0)
    and res=ref (makevector(NUMOUT,0.0))
  in
    count := read_int f;
    NUMIN := read_int f;

    i := 0;
    while i < count do
    (
      k := 0;
      while k < NUMIN do
      (
        inp[k]= read_real f;
        k := k + 1
      );
      print "Вход: ";
      printvector inp;
      res := getoutput(inp);
      print " Полученный выход: ";
      printvector(res);
      if Int.rem(i,10)=0 then
      (
       print "Press Enter to continue";
       TextIO.input1(TextIO.stdIn);
       ()
      )
      else
      ();
      i := i + 1
    );
    TextIO.closeIn(f)
  end
end

val _=main()
*/