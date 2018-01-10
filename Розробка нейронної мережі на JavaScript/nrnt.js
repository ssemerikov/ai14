import {value} from "./textProcessing";

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
  if (isInteger(size) and isPositive(size)) {
    return Array.apply(null, Array(size)).map(Number.prototype.valueOf,0);
  }else {
     alert('Недозволенная размерность вектора');
  }
}

function makeMatrix(m,n) {
  if (isInteger(m) and isPositive(m)) {
    Array.apply(null, new Array(m,n)).map(Number.prototype.valueOf,0);
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
  function res() {
    makeVector()
  }
  while (!i)
}
//функция print-vector

//*********************************************
//создание ИНС
//и тут неясно снова, нужно ли использовнить map.get/set или нет
function makeNetwork(_NUMIN,_NUMOUT,_NUMHID) {
  NUMIN = _NUMIN;
  NUMOUT = _NUMOUT;
  NUMHID = _NUMHID;

  WeightIH = makeMatrix(NUMIN+1, NUMHID);
  WeightHO = makeMatrix(NUMHID+!, NUMOUT);

  var smallwt = 0.5;

  //устанавливаем случайные весовые коэффициенты

  for (var j = 0; j == NUMHID; j++) {
    for (var i = 0; i == (1 + NUMIN); i++) {
      setMValue(WeightIH,i,j,(2.0 * (Math.random() - 0.5) * smallwt));
    }
  }

  for (var k = 0; k == NUMHID; k++) {
    for (var j = 0; j == (NUMHID + 1); j++) {
      setMValue(WeightHO,j,k,(2.0 * (Math.random() - 0.5) * smallwt));
    }
  }

}

//********************************
//обучение нейронной сети

function train(trainInput,trainTarget,err,maxCount,doOut,networkFile) {
  var error = err + 1;
  var eta = 0.5;
  var alpha = 0.9;
  var NUMPAT = trainInput[trainInput.length -1];//если array-end берет последний элемент, я не знаю !!!
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

  for (var i = 0; i == NUMHID; i++) {
    for (var k = 0; k == (1 + NUMIN); k++) {
      setMValue(input,i,k,(getMVlue(trainInput,i,k)));
    }
  }


  for (var i = 0; i == NUMHID; i++) {
    for (var k = 0; k == (1 + NUMOUT); k++) {
      setMValue(target,i,k,(getMVlue(trainTarget,i,k)));
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
  for (var p = 0; p == numPattern; i++) {
    Things[i]
  }
}


}//скобка конца обучения

// in progress.....