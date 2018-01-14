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
      WeightIH[i][j]=2.0*(Math.random()-0.5)*smallwt;
  }

  for(var k=0;k < NUMOUT;k++)
  {
    for(var j=0;j < NUMHID+1;j++)
      WeightHO[j][k]=2.0*(Math.random()-0.5)*smallwt;
  }
}


/*с помощью этих функций можно сохранять и 
  загружать матрицы весовых коэффициентов*/

function getfiledata(filename)
{
  var fs = require('fs');
  var contents = fs.readFileSync(filename, 'utf8');

  var str = contents.replace(/\r/g, ' ');
  str = str.replace(/\n/g, ' ');
  str = str.replace(/\t/g, ' ');
  var arr=str.split(" ");
  var res=[];
  var j=0;
  for(var i=0;i<arr.length;i++)
    if(arr[i]!="")
      res[j++]=parseFloat(arr[i]);
  return res;
}


function readnetwork(filename)
{
  var data=getfiledata(filename);
  var pos=0;
 
  NUMIN = data[pos++];
  NUMOUT = data[pos++];
  NUMHID = data[pos++];

  makenetwork(NUMIN,NUMOUT,NUMHID);

  for(var i = 0;i < NUMIN+1;i++)
    for(var k = 0;k < NUMHID;k++)
      WeightIH[i][k]=data[pos++];
  
  for(var i = 0;i < NUMHID+1;i++)
    for(var k = 0;k < NUMOUT;k++)
      WeightHO[i][k]=data[pos++];
  mini = data[pos++];
  maxi = data[pos++];
  mino = data[pos++];
  maxo = data[pos++];
  GlobalMinError = data[pos++];
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


/*обучение нейронной сети*/
function train(TrainInput,TrainTarget,Err,MaxCount,DoOut,NetworkFile)
{
  var NUMPAT=TrainInput.length; /*число обучающих шаблонов*/
  var Error=Err+1.0;
  var eta=0.5;
  var alpha=0.9;
  var ranpat=makevector(NUMPAT,0);
  var NumPattern=NUMPAT;
  var NumInput=NUMIN;
  var NumHidden=NUMHID;
  var NumOutput=NUMOUT;
         /*временные массивы*/
  var DeltaWeightIH=makematrix(1+ NUMIN,NUMHID, 0.0);
  var DeltaWeightHO=makematrix(1+ NUMHID,NUMOUT, 0.0);
  var SumDOW=makevector(NUMHID,0.0);
  var DeltaH=makevector(NUMHID,0.0);
  var DeltaO=makevector(NUMOUT,0.0);
  var SumH=makevector(NUMHID,0.0);
  var Hidden=makevector(NUMHID,0.0);
  var SumO=makevector(NUMOUT,0.0);
  var Output=makevector(NUMOUT,0.0);
  var Input=makematrix(NUMPAT,NUMIN, 0.0);
  var Target=makematrix(NUMPAT,NUMOUT, 0.0);
    
    /*копируем тренировочные матрицы во временные во избежание порчи*/
  for(var i=0;i < NUMPAT;i++)
    for(var k=0;k < NUMIN;k++)
      Input[i][k]=TrainInput[i][k];

  for(var i=0;i < NUMPAT;i++)
    for(var k=0;k < NUMOUT;k++)
      Target[i][k]=TrainTarget[i][k];
    /*если существует файл NetworkFile - загрузим его для дообучения*/

  if(require('fs').existsSync(NetworkFile))
    readnetwork(NetworkFile);
  else
  {
     mini = Input[0][0];
     maxi = Input[0][0];
     mino = Target[0][0];
     maxo = Target[0][0];
  
     /*поиск граничных значений в числовых массивах*/
     for(var i = 0;i < NumPattern;i++)
     {
       for(var k = 0;k < NumInput;k++)
       {
         if(mini > Input[i][k])
           mini = Input[i][k];
         if (maxi < Input[i][k])
           maxi = Input[i][k];
       }
       for(var k = 0;k < NumOutput;k++)
       {
         if(mino > Target[i][k])
           mino = Target[i][k];
         if(maxo < Target[i][k])
           maxo = Target[i][k];
       }
     }
  }
  
  /*нормализация*/
  for(var i = 0;i < NumPattern;i++)
  {         
    for(var k = 0; k < NumInput;k++)
      Input[i][k]=(Input[i][k] - mini) / (maxi - mini);
    for(var k = 0;k < NumOutput;k++)
      Target[i][k]=(Target[i][k] - mino) / (maxo - mino);
  }              

  /*цикл обучения по достижению заданной ошибки или числа итераций*/
  for(var epoch = 0;epoch < MaxCount && GlobalMinError >= Err;epoch++)
  {
    /*перемешиваем шаблоны*/
    for(var p=0;p<NumPattern;p++)
      ranpat[p]=Math.floor(Math.random()*NumPattern); 
    Error = 0.0;
    /*цикл обучения по шаблонам*/
    for(var np = 0;np<NumPattern;np++)
    {
      /*выбираем шаблон*/
      p = ranpat[np];
      /*активация скрытого слоя*/
      for(var j = 0;j < NumHidden;j++)
      {
        SumH[j]=WeightIH[0][j];
        for(var i = 0;i < NumInput;i++)
          SumH[j]+=Input[p][i]*WeightIH[1+i][j];
        Hidden[j]=1.0/(1.0+Math.exp(-SumH[j]));
      }
      /*активация выходного слоя и вычисление ошибки*/
      for(var k = 0;k < NumOutput;k++)
      {
        SumO[k]=WeightHO[0][k];
        for(var j = 0;j < NumHidden;j++)
          SumO[k]+=Hidden[j]*WeightHO[1+j][k];
        Output[k]=1.0/(1.0+Math.exp(-SumO[k]));
        Error+=0.5*(Target[p][k]-Output[k])*(Target[p][k]-Output[k]);
        DeltaO[k]=(Target[p][k]- Output[k])*Output[k]*(1.0-Output[k]);
      }
      /*обратное распространение ошибки на скрытый слой*/
      for(var j = 0;j < NumHidden;j++)
      {
        SumDOW[j]=0.0;
        for(var k = 0;k < NumOutput;k++)
          SumDOW[j]+=WeightHO[1+j][k]*DeltaO[k];
        DeltaH[j]=SumDOW[j]*Hidden[j]*(1.0 -Hidden[j]);
      }
      for(var j = 0;j < NumHidden;j++)
      {
        DeltaWeightIH[0][j]=eta*DeltaH[j]+alpha*DeltaWeightIH[0][j];
        WeightIH[0][j]=WeightIH[0][j]+DeltaWeightIH[0][j];
        for(var i = 0;i < NumInput;i++)
        {
          DeltaWeightIH[1+i][j]=eta*Input[p][i]*DeltaH[j]+alpha*DeltaWeightIH[1+i][j];
          WeightIH[1+i][j]=WeightIH[1+i][j]+DeltaWeightIH[1+i][j];
        }
      }
      for(var k = 0;k < NumOutput;k++)
      {
        DeltaWeightHO[0][k]=eta*DeltaO[k]+alpha*DeltaWeightHO[0][k];
        WeightHO[0][k]=WeightHO[0][k]+DeltaWeightHO[0][k];
        for(var j = 0;j < NumHidden;j++)
        {
          DeltaWeightHO[1+j][k]=eta*Hidden[j]*DeltaO[k]+alpha*DeltaWeightHO[1+j][k];
          WeightHO[1+j][k]=WeightHO[1+j][k]+DeltaWeightHO[1+j][k];
        }
      }
    }
      
    if(DoOut && epoch%100 == 0) /*отладочный вывод*/
      console.log("epoch=" +epoch+", error="+Error);
    if(Error < GlobalMinError)
    {
      GlobalMinError = Error;
      console.log("epoch=" +epoch+", (min)error="+Error);
      writenetwork(NetworkFile);
    }
  }
  writenetwork(NetworkFile);
  return Error;
}


/*подача сигнала на вход сети и получение результата*/
function getoutput(BeInput)
{
  var Input=makevector(NUMIN,0.0);
  var Output=makevector(NUMOUT,0.0);
  var result=makevector(NUMOUT,0.0);
  var SumH=makevector(NUMHID,0.0);
  var Hidden=makevector(NUMHID,0.0);
  var SumO=makevector(NUMOUT,0.0);
  var NumInput=NUMIN;
  var NumHidden=NUMHID;
  var NumOutput=NUMOUT;
  /*нормализация входа*/
  for(var k = 0;k < NumInput;k++)
    Input[k]=(BeInput[k]- mini)/(maxi - mini);
  
  /*активация скрытого слоя*/
  for(var j = 0;j < NumHidden;j++)
  {
    SumH[j]=WeightIH[0][j];
    for(var i = 0;i < NumInput;i++)
      SumH[j]+=Input[i]*WeightIH[1+i][j];
    Hidden[j]=1.0/(1.0+Math.exp(-SumH[j]));
  }
  
  /*активация выходного слоя*/
  for(var k = 0;k < NumOutput;k++)
  {
    SumO[k]=WeightHO[0][k];
    for(var j = 0;j < NumHidden;j++)
      SumO[k]+=Hidden[j]*WeightHO[1+j][k];
    Output[k]=1.0/(1.0+Math.exp(-SumO[k]));
  }
  
  /*денормализация выхода*/
  for(var k = 0;k < NumOutput;k++)
    result[k]= Output[k]*(maxo - mino)+ mino;
  return result;
}

/*пример создания использования нейронной сети*/

function readenter()
{
  var buf=new Buffer([0]);
  var s=require('fs').readSync(process.stdin.fd, buf, 0, 1);
}

function main()
{
  var NUMPAT=60; /*число обучающих шаблонов - может переопределяться в файле*/
  var data=getfiledata("etalons.txt");
  var pos=0;

  /*форматы файлов матриц: число_строк число_столбцов данные*/
  NUMPAT = data[pos++];
  NUMIN = data[pos++];
  NUMOUT = data[pos++];
  
  NUMHID = NUMIN * 2 + 1; /*число нейронов в скрытом слое*/

  var Input=makematrix(NUMPAT,NUMIN,0.0);
  var Output=makematrix(NUMPAT,NUMOUT,0.0);
  var inp=makevector(NUMIN,0.0);
  var res=makevector(NUMOUT,0.0);
  var out=makevector(NUMOUT,0.0);

  for(var i =0;i < NUMPAT;i++)
  {
    for(var k = 0;k < NUMIN;k++)
      Input[i][k]=data[pos++];
    for(var k = 0;k < NUMOUT;k++)
      Output[i][k]=data[pos++];
  }
  
  makenetwork(NUMIN,NUMOUT,NUMHID);
  console.log("Размерность входа - " +NUMIN+ ", размерность выхода - " +NUMOUT+
        ", число шаблонов - " +NUMPAT);
  train(Input,Output,0.000001,150000, true, "network.txt");
  /* readnetwork("network.txt"); */
  
  console.log("Исходные данные:");
  for(var i = 0;i < NUMPAT;i++)
  {
    process.stdout.write("Вход: ");
    printvector (vectorfrommatrix(Input,i));
    res = getoutput(vectorfrommatrix(Input, i));
    process.stdout.write(" Эталонный выход: ");
    printvector (vectorfrommatrix(Output, i));
    process.stdout.write(" Полученный выход: ");
    printvector(res);
    if(i%10==0)
    {
      console.log("Press Enter to continue");
      readenter();
    }
  }
  console.log("Тестовые данные:");

  var data=getfiledata("test.txt");
  var pos=0;

  count = data[pos++];
  NUMIN = data[pos++];

  for(var i = 0;i < count;i++)
  {
    for(var k = 0;k < NUMIN;k++)
      inp[k]= data[pos++];
    process.stdout.write("Вход: ");
    printvector(inp);
    res = getoutput(inp);
    process.stdout.write(" Полученный выход: ");
    printvector(res);
    if(i%10==0)
    {
      console.log("Press Enter to continue");
      readenter();
    }
  }

}


main();

