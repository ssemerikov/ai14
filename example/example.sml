(*трехлойная нейронная сеть (вход, скрытый слой, выход*)

val NUMIN=ref 0   (*размерность входа*)
and NUMHID=ref 0  (*размерность скрытого слоя*)
and NUMOUT=ref 0 (*размерность выхода*)

(*матрицы весовых коэффициентов*)
and WeightIH=ref (Array2.array(0,0,0.0)) (*соединения входа и скрытого слоя*)
and WeightHO=ref (Array2.array(0,0,0.0)) (*соединения скрытого слоя и выходa*)

(*для нормализации*)
and mini=ref 0
and maxi=ref 0
and mino=ref 0
and maxo=ref 0
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

val _ = (
  let
    val m=makevector(10)
  in 
    printvector m;
    makenetwork (10, 5 ,2)
  end
)