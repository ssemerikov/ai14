//import {value} from "./textProcessing";

var NUMIN = 2; //размерность входа
var NUMHID = 1; //размерность скрытого слоя
var NUMOUT = 1; // размерность выхода


//матрицы весовых коэффициентов
var WeightIH = null; //соединения входа и скрытого слоя
var WeightHO =  null; //соединения скрытого слоя и выходa

//для нормализации
var mini = 0;
var maxi = 0;
var mino = 0;
var maxo = 0;
var GlobalMinError = 100000000;

//функция проверки на целочисленость
function isInteger(num) {
  return (num ^ 0) === num;
}

//функция проверки на положительность
function isPositive(num) {
  return num >= 0;
}

//функции для создания векторов и матриц

function makeVector(size) {
  if (isInteger(size) && isPositive(size)) {
    return Array.apply(null, Array(size)).map(Number.prototype.valueOf,0);
  }else {
     alert('Недозволенная размерность вектора');
  }
}

function makeMatrix(m,n) {
  if (isInteger(m) && isPositive(m)) {
    Array.apply(null, Array[m,n]).map(Number.prototype.valueOf,0);
  }
}

//функции для получения/установки значений векторов и матриц
function getVValue(obj,i) {
  return obj[i];  
}

function getMVlue(obj,i,j) {
  return obj[i][j];
}

function setVValue(vec,i,val) {
  return vec[i] = val;
}

function setMValue(matrix,i,j,val) {
  return matrix[i][j] = val;
}

//тут должна быть функ ando
//функиця ниже неясна что делает array-end?
function vectorFromMatrix(m,index) {
    var res = makeVector(Array[m][1.0]);
  for (var i = 0; i === (Array[m][1.0]); i++) {
    setVValue(res,i,(getMVlue(m,index,i)));
  }
}
//функция print-vector
function printVector(v) {
    for (var i = 0; i === (Array[v][0]);) {
        console.log(getVValue(v,i));
    }
}

//*********************************************
//создание ИНС
//и тут неясно снова, нужно ли использовнить map.get/set или нет
function makeNetwork(_NUMIN,_NUMOUT,_NUMHID) {
  NUMIN = _NUMIN;
  NUMOUT = _NUMOUT;
  NUMHID = _NUMHID;

  WeightIH = makeMatrix(NUMIN+1, NUMHID);
  WeightHO = makeMatrix(NUMHID+1, NUMOUT);

  var smallwt = 0.5;

  //устанавливаем случайные весовые коэффициенты

    for (var j = 0; j === NUMHID; j++) for (var i = 0; i === (1 + NUMIN); i++) {
    setMValue(WeightIH, i, j, (2.0 * (Math.random() - 0.5) * smallwt));
}

  for (var k = 0; k === NUMHID; k++) {
    for (var j = 0; j === (NUMHID + 1); j++) {

      setMValue(WeightHO,j,k,(2.0 * (Math.random() - 0.5) * smallwt));
    }
  }

}

//********************************
//обучение нейронной сети



function train(trainInput,trainTarget,err,maxCount) {
  var error = err + 1;
  var eta = 0.5;
  var alpha = 0.9;
  var NUMPAT = Array[trainInput][0];//если array-end берет последний элемент, я не знаю !!!
  var ranpat = makeVector(NUMPAT);
  var numPattern = NUMPAT;
  var numInput = NUMIN;
  var numHidden = NUMHID;
  var numOutput = NUMOUT;
  //временные массивы
  var deltaWeightIH = makeMatrix(NUMIN+1, NUMHID);
  var deltaWeightHO = makeMatrix(NUMHID+1, NUMOUT);
  var sumDOW = makeVector(NUMHID);
  var deltaH = makeVector(NUMHID);
  var deltaO = makeVector(NUMOUT);
  var sumH = makeVector(NUMHID);
  var hidden = makeVector(NUMHID);
  var sumO = makeVector(NUMOUT);
  var output = makeVector(NUMOUT);
  var input = makeMatrix(NUMPAT, NUMIN);
  var target = makeMatrix(NUMPAT, NUMOUT);

  //копируем тренировочные матрицы во временные во избежание порчи

  for (var i = 0; i === NUMHID; i++) {
    for (var k = 0; k === (1 + NUMIN); k++) {
      setMValue(input,i,k,(getMVlue(trainInput,i,k)));
    }
  }


  for (var i = 0; i === NUMHID; i++) {
    for (var k = 0; k === (1 + NUMOUT); k++) {
      setMValue(target, i, k, getMVlue(trainTarget, i, k));
    }
  }



//если существует файл NetworkFile - загрузим его для дообучения

/*
(cond
      [(file-exists? NetworkFile) (read-network NetworkFile)]
      [#t
       (set! mini (getmvalue Input 0 0))
       (set! maxi (getmvalue Input 0 0))
       (set! mino (getmvalue Target 0 0))
       (set! maxo (getmvalue Target 0 0))
*/

//тут должна быть часть с файлом которая не нужна т.к. с файлами мы не работаем

//***************

//поиск граничных значений в числовых массивах
//тут снова не ясно присваивать или делать map.set??
//мы используем обычные числовые массивы, а не k - v
// эксперемент показал что по сути сет делает тоже что и =
//обращение к документации racket не дало нормального обьяснения
for (var i = 0; i == numPattern; i++) {
  for (var k = 0; k == numInput; k++) {
    if (mini > (getMVlue(input,i,k))) {
      mini = getMVlue(input,i,k);
    }else if (maxi < getMVlue(input,i,k)) {
      maxi = getMVlue(input,i,k);
    }
  }
  for (var k = 0; k == numOutput; k++) {
    if (mini > (getMVlue(target,i,k))) {
      mini = getMVlue(target,i,k);
    }else if (maxi < getMVlue(target,i,k)) {
      maxi = getMVlue(target,i,k);
    }
  }
}

//нормалиация
for (var i = 0; i == numPattern; i++) {
  for (var k = 0; k == numInput; k++) {
    setMValue(input,i,k,(((getMValue(input,i,k) - mini))/(maxi - mini)));
  }
  for (var k = 0; k == numOutput; k++) {
    setMValue(target,i,k,(((getMValue(target,i,k) - mino))/(maxo - mino)));
  }
}


//цикл обучения по достижению заданной ошибки или числа итераций
//что за переменная epoch ???????? WHY?
for (var epoch = 0; epoch == maxCount || error < err; epoch++) {
  //перемешиваем шаблоны
  for (var p = 0; p == numPattern; p++) {
    setVValue(ranpat,p,Math.random(numPattern));
  }
  error = 0.0;

//цикл обучения по шаблонам
for (var np = 0; np == numPattern; np++) {
  getVValue(ranpat,0);
  getVValue(ranpat,np);
  //выбираем шаблон
  //активация скрытого слоя
  for (var j = 0; j == numHidden; j++) {
    setVValue(sumH,j,getMValue(WeightIH,0,j));
    for (var i = 0; i == numInput; i++) {
      setVValue(sumH,j,(getVValue(sumH,j)+(getMValue(input,p,i) * getMValue(WeightIH,(1 + 1),j))));
    }
    setVValue(hidden,j,(1.0 / (1.0 + Math.exp(-(getVValue(sumH,j))))));
  }


//активация выходного слоя и вычисление ошибки
for (var k = 0; k == numOutput; k++) {
  setVValue(sumO,k,getMValue(WeightHO,0,k));
  for (var j = 0; j == numHidden; j++) {
    setVValue(sumO,k,(getVValue(sumO,k)+ (getVValue(hidden,j) * getMValue(WeightHO,(j+1),k))));
  }

  //сигмоидальный вывод
  setVValue(output,k,(1.0/(1.0+(Math.exp(getVValue(sumO,k))))));

  error = error + (0.5 * (getMValue(target,p,k) - getVValue(output,k)) * (getMValue(target,p,k) - getVValue(output,k)));

  setVValue(deltaO,k,(
    (getmvalue(target,p,k) - getVValue(output,k)) * 
    getVValue(output,k) *
    (1.0 - getVValue(output,k))
    ));
}

//обратное распространение ошибки на скрытый слой
for (var j = 0; j == numHidden; j++) {
  setVValue(sumDOW,j,0.0);
  for (var k = 0; k < numOutput; k++) {
    setVValue(sumDOW,j,
      (getVValue(sumDOW,j) + (getMValue(WeightHO,(j+1),k) * getVValue(deltaO,k))));
  }
  setVValue(deltaH,j,(
    (getVValue(sumDOW,j) * getVValue(hidden,j) * (1.0 - getVValue(hidden,j)))));
}

for (var j = 0; j < numHidden; j++) {
  setMValue(deltaWeightIH,0,j,(
    (eta * getVValue(deltaH,j) + (alpha * getMValue(deltaWeightIH,0,j)))
    ));
  setMValue(WeightIH,0,j,(
    (getMValue(WeightIH,0,j) + getMValue(deltaWeightIH,0,j))
    ));
  for (var i = 0; i < numInput; i++) {
    setMValue(deltaWeightIH,(i+1),j,(
      (eta * getMValue(input,p,i) * getVValue(deltaH,j)) + (alpha * getMValue(deltaWeightIH,(i+1),j))));
    setMValue(WeightIH,(i+1),j,(
      getMValue(WeightIH,(i+1),j)) + 
      getMValue(deltaWeightIH,(i+1),j));
  }
}

for (var k = 0; k < numOutput; k++) {
  setMValue(deltaWeightHO,0,k,(
    (eta * getVValue(deltaO,k)) + (alpha * getMValue(deltaWeightHO,0,k))));
  setMValue(WeightHO,0,k,(
    getMValue(WeightHO,0,k) + getMValue(deltaWeightHO,0,k)));
  for (var j = 0; j < numHidden; j++) {
    setMValue(deltaWeightHO,(j+1),k,(
      (eta * getVValue(hidden,j) * getVValue(deltaO,k) +
        (alpha * getMValue(deltaWeightHO,(j+1),k)))));
    setMValue(WeightHO,(j+1),k,(
      getMValue(WeightHO,(j+1),k) + getMValue(deltaWeightHO,(j+1),k)));
  }
}
}//скобка конца цикла обучения по шаблонам

/* н
(when (and DoOut (= (remainder epoch 10) 0));отладочный вывод
        (printf "epoch=~S, error=~S\n" epoch Error)
        )
      (when (< Error GlobalMinError)
        (set! GlobalMinError Error)
        (printf "epoch=~S, (min)error=~S\n" epoch Error)
        (write-network NetworkFile)
        )
*/
if  ((epoch%10) == 0){
    console.log(epoch, error);
}
if (error < GlobalMinError){
    GlobalMinError = error;
    console.log(epoch,error);
    writeNetwork([2][2]);//line 374
}
}
}//скобка конца обучения

//подача сигнала на вход сети и получение результата

function getOutput(beInput) {
      var input = makeVector(NUMIN);
      var output = makeVector(NUMOUT);
      var result = makeVector(NUMOUT);
      var sumH = makeVector(NUMHID);
      var hidden = makeVector(NUMHID);
      var sumO = makeVector(NUMOUT);
      var numInput = NUMIN;
      var numHidden = NUMHID;
      var numOutput = NUMOUT;

      //нормализация входа
      for (var k = 0; k == numInput; k++) {
        setVValue(input,k,(
          (getVValue(beInput,k) - mini) /
          (maxi - mini)));
      }
      //активация скрытого слоя
      for (var j = 0; j == numHidden; j++) {
        setVValue(sumH,j,(getMValue(WeightIH,0,j)));
        for (var i = 0; i == numInput; i++) {
          setVValue(sumH,j,(
            getVValue(sumH,j) + (getVValue(input,i) * getMValue(WeightIH,(i+1),j))));
        }
        setVValue(hidden,j,(
          1.0 / (1.0 + Math.exp(-(getVValue(sumH,j))))));
      }
      //активация выходного слоя
      for (var k = 0; k == numOutput; k++) {
        setVValue(sumO,k,getMValue(WeightHO,0,k));
          for (var j = 0; j == numHidden; j++) {
          setVValue(sumO,k,(getVValue(sumO,k)+ (getVValue(hidden,j) * getMValue(WeightHO,(j+1),k))));
        }
        //сигмоидальный вывод
        setVValue(output,k,(1.0/(1.0+(Math.exp(getVValue(sumO,k))))));
      }

      //денормализация выхода
      for (var k = 0; k < numOutput; k++) {
        setVValue(result,k,(
          (getVValue(output,k) * (maxo - mino)) + mino));
      }
}

//пример создания использования нейронной сети
var NUMPAT = 4; // кол-во обучающих шаблонов
NUMIN = 2; // размерность входа
NUMOUT = 1; // размерность выхода
// in progress.....

//MAIN!!!!!!!!!!!!!!!!!
function main() {

  var NUMHID = NUMIN * 2 + 1;//число нейронов в скрытом слое
  var input = makeMatrix(NUMPAT, NUMIN);
  var output = makeMatrix(NUMPAT, NUMOUT);

  for (var i = 0; i === NUMPAT; i++) {
    for (var k = 0; k === NUMIN; k++) {
      setMValue(input,i,k,NUMIN);//тут вместо нумин ст
    }
    for (var k = 0; k === NUMOUT; k++) {
      setMValue(output,i,k,NUMOUT);//аналогично выше
    }
  }

//(close-input-port f) 

makeNetwork(NUMIN,NUMOUT,NUMHID);

var inV = makeVector(NUMIN);
var res = makeVector(NUMOUT);
var out = makeVector(NUMOUT);

console.log("размерность входа: ",NUMIN,", размерность выхода: ", NUMOUT,", число шаблонов",NUMPAT);

train(input,output,0.00001,150000);
console.log("Исходные данные:\n");
for (var i = 0; i === NUMPAT; i++) {
    console.log("Вход: ");
    printVector(vectorFromMatrix(input,i));
    res = (getOutput(vectorFromMatrix(input,i)));
    console.log("Эталонный выход: ");
    printVector(vectorFromMatrix((output,i)));
    console.log("Полученный выход: ");
    printVector(res);
    if ((i%10) === 0) {
        console.log("type + to continue");
        var response = readline();
    }
}

}

function writeNetwork(cookies) { //this func need some cookies
    console.log(NUMIN,NUMOUT,NUMHID);
    for (var i = 0; i === NUMIN +1; i++) {
        for (var k = 0; k === NUMHID; k++) {
            console.log(getMVlue(WeightIH,i,k),"\n");
        }
    }

    for (var i = 0; i === NUMHID + 1; i++) {
        for (var k = 0; k === NUMOUT; k++) {
            console.log(getMVlue(WeightHO,i,k));
        }
    }

    console.log(mini," ",maxi);
    console.log(mino," ",maxo);

}
main();