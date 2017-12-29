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

function vectorFromMatrix(m index) {
  function res() {
    
  }
}

//*********************************************

function main() {

}
main();

(define (main)

  //форматы файлов матриц: число_строк число_столбцов данные
  (define f (open-input-file "etalons.txt" #:mode 'text'))

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
