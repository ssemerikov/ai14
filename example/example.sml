(*��嫮���� ���஭��� ��� (�室, ����� ᫮�, ��室*)

val NUMIN=ref 0   (*ࠧ��୮��� �室�*)
and NUMHID=ref 0  (*ࠧ��୮��� ���⮣� ᫮�*)
and NUMOUT=ref 0 (*ࠧ��୮��� ��室�*)

(*������ ��ᮢ�� �����樥�⮢*)
and WeightIH=ref (Array2.array(0,0,0.0)) (*ᮥ������� �室� � ���⮣� ᫮�*)
and WeightHO=ref (Array2.array(0,0,0.0)) (*ᮥ������� ���⮣� ᫮� � ��室a*)

(*��� ��ଠ����樨*)
and mini=ref 0
and maxi=ref 0
and mino=ref 0
and maxo=ref 0
and GlobalMinError=ref 100000000

exception SizeError of string

fun random()=
  Random.random(Random.newgen())

(*�㭪樨 ��� ᮧ����� ����஢ � �����*)

fun makevector(size)=
  if size>0 then
      Array.array(size,0.0)
  else
      raise SizeError "������������� ࠧ��୮��� �����"

fun makematrix(m,n)=
  if m>0 andalso n>0 then
      Array2.array(m, n, 0.0)
  else
      raise SizeError "������������� ࠧ��୮��� ������"


fun vectorfrommatrix(m,index)=
  Array2.row(m,index)

(*�㭪樨 ��� ����祭��/��⠭���� ���祭�� ����஢ � �����*)

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

(*ᮧ����� ���஭��� ��*)

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

  (*��⠭�������� ��砩�� ��ᮢ� �����樥���*)
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