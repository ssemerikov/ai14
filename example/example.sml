(*��嫮���� ���஭��� ��� (�室, ����� ᫮�, ��室*)

val NUMIN=ref 0   (*ࠧ��୮��� �室�*)
and NUMHID=ref 0  (*ࠧ��୮��� ���⮣� ᫮�*)
and NUMOUT=ref 0 (*ࠧ��୮��� ��室�*)

(*������ ��ᮢ�� �����樥�⮢*)
and WeightIH=ref (Array2.array(0,0,0.0)) (*ᮥ������� �室� � ���⮣� ᫮�*)
and WeightHO=ref (Array2.array(0,0,0.0)) (*ᮥ������� ���⮣� ᫮� � ��室a*)

(*��� ��ଠ����樨*)
and mini=ref 0.0
and maxi=ref 0.0
and mino=ref 0.0
and maxo=ref 0.0
and GlobalMinError=ref 100000000.0

exception SizeError of string

fun random()=
  Random.random(Random.newgen())

(*�㭪樨 ��� ᮧ����� ����஢ � �����*)

fun makevector(size, value)=
  if size>0 then
      Array.array(size,value)
  else
      raise SizeError "������������� ࠧ��୮��� �����"

fun makematrix(m,n,value)=
  if m>0 andalso n>0 then
      Array2.array(m, n, value)
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
  
  WeightIH := makematrix(1 + !NUMIN, !NUMHID, 0.0);
  WeightHO := makematrix(1 + !NUMHID, !NUMOUT, 0.0);

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

(*� ������� ��� �㭪権 ����� ��࠭��� � 
  ����㦠�� ������ ��ᮢ�� �����樥�⮢*)

fun int_from_stream stream =
  Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream)

fun real_from_stream stream =
  Option.valOf (TextIO.scanStream (Real.scan) stream)

fun readnetwork(filename)=
let
  val f=TextIO.openIn(filename)
  and i=ref 0
  and k=ref 0
in
  NUMIN := int_from_stream f;
  NUMOUT := int_from_stream f;
  NUMHID := int_from_stream f;
  makenetwork(!NUMIN,!NUMOUT,!NUMHID);
  i := 0;
  while !i < !NUMIN+1 do
  (   
    k := 0;
    while !k < !NUMHID do
    (
      setmvalue(!WeightIH,!i,!k, real_from_stream f);
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
      setmvalue(!WeightHO,!i,!k, real_from_stream f);
      k := !k + 1
    );
    i := !i + 1
  );
  mini := real_from_stream f;
  maxi := real_from_stream f;
  mino := real_from_stream f;
  maxo := real_from_stream f;
  GlobalMinError := real_from_stream f;
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

(*���祭�� ���஭��� ��*)
fun train(TrainInput,TrainTarget,Err,MaxCount,DoOut,NetworkFile)=
let
  val NUMPAT=Array2.nRows(TrainInput) (*�᫮ ������� 蠡�����*)
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
         (*�६���� ���ᨢ�*)
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
    
    (*�����㥬 �७�஢��� ������ �� �६���� �� ��������� ����*)
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
    (*�᫨ ������� 䠩� NetworkFile - ����㧨� ��� ��� �����祭��*)

    if FileSys.access(NetworkFile,[]) then 
      readnetwork(NetworkFile)
    else
    (
       mini := getmvalue(Input,0,0);
       maxi := getmvalue(Input,0,0);
       mino := getmvalue(Target,0,0);
       maxo := getmvalue(Target,0,0);
    
       (*���� �࠭���� ���祭�� � �᫮��� ���ᨢ��*)
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
    
    (*��ଠ������*)
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

    (*横� ���祭�� �� ���⨦���� �������� �訡�� ��� �᫠ ���権*)
    epoch := 0;
    while !epoch < MaxCount andalso !Error<Err do
    let
      val p=ref 0
      and gen=Random.newgen()
      and np=ref 0
      and j=ref 0
    in
      (*��६�訢��� 蠡����*)
      while !p<NumPattern do
      (
        setvvalue(ranpat,!p,Random.range(0,NumPattern) gen); 
        p := !p + 1
      );
      Error := 0.0;
      (*横� ���祭�� �� 蠡�����*)
      np := 0;
      while !np<NumPattern do
      (
        (*�롨ࠥ� 蠡���*)
        p := getvvalue(ranpat,!np);
        (*��⨢��� ���⮣� ᫮�*)
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
        (*��⨢��� ��室���� ᫮� � ���᫥��� �訡��*)
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
        (*���⭮� �����࠭���� �訡�� �� ����� ᫮�*)
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
        
      if DoOut andalso Int.rem(!epoch, 10) = 0 then (*�⫠���� �뢮�*)
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

(*����� ᨣ���� �� �室 �� � ����祭�� १����*)
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
    (*��ଠ������ �室�*)
    k := 0;
    while !k < NumInput do
    (
      setvvalue(Input,!k, (getvvalue(BeInput,!k)- !mini)/(!maxi - !mini));
      k := !k +1
    );
    
    (*��⨢��� ���⮣� ᫮�*)
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
    
    (*��⨢��� ��室���� ᫮�*)
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
    
    (*����ଠ������ ��室�*)
    k := 0;
    while !k < NumOutput do
    (
      setvvalue(result,!k, getvvalue(Output,!k)*(!maxo - !mino)+ !mino);
      k := !k +1
    )
end


(*

(*�ਬ�� ᮧ����� �ᯮ�짮����� ���஭��� ��*)

(define NUMPAT 60) ;�᫮ ������� 蠡����� - ����� ��८�।������� � 䠩��
;(set! NUMIN  4)  ;ࠧ��୮��� �室� - ����� ��८�।������� � 䠩��
;(set! NUMOUT 1)  ;ࠧ��୮��� ��室� - ����� ��८�।������� � 䠩��

(define (main)
  
  ;�ଠ�� 䠩��� �����: �᫮_��ப �᫮_�⮫�殢 �����
  (define f (open-input-file "etalons.txt" #:mode 'text))
  
  (set! NUMPAT (read f))
  (set! NUMIN (read f))
  (set! NUMOUT (read f))

  (set! NUMHID (+ ( * NUMIN 2) 1)) ;�᫮ ���஭�� � ���⮬ ᫮�
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
   "�����୮��� �室� - ~S, ࠧ��୮��� ��室� - ~S, �᫮ 蠡����� - ~S\n"
   NUMIN NUMOUT NUMPAT
   )
  
  (train Input Output 0.00001 150000 #t "network.txt")
  ;(read-network "network.txt")
  
  (printf "��室�� �����:\n")
  (do [(i 0 (+ 1 i)) (res null)] ((= i NUMPAT))
    (printf "�室: ")
    (print-vector (vector-from-matrix Input i))
    (set! res (getoutput (vector-from-matrix Input i)))
    (printf " �⠫���� ��室: ")
    (print-vector (vector-from-matrix Output i))
    (printf " ����祭�� ��室: ")
    (print-vector res)
    (when (zero? (remainder i 10))
      (printf "Press Enter to continue")
      (read-char)
      )
    )
  
  (printf "���⮢� �����:\n")
  (set! f (open-input-file "test.txt" #:mode 'text))

  (define count (read f))
  (set! NUMIN (read f))
  (do [(i 0 (+ 1 i)) (res null)] ((= i count))
    (do [(k 0 (+ 1 k))] ((= k NUMIN))        
      (setvvalue in k (read f))
      )
    (printf "�室: ")
    (print-vector in)
    (set! res (getoutput in))
    (printf " ����祭�� ��室: ")
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