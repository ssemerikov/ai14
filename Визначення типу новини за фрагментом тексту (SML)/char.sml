fun ordList [] = []
   | ordList (x::xs) = ord(x)::(ordList xs);(* кодируем весь список*)
   
   fun printList xs = print(String.concatWith " " (map Int.toString xs));(*функция для вывода списка без , *)
   
   fun main(str)=
   let
   	val mystring = explode(str)(*из строки получаем чар лист*)
	and sizeStr = 1024
	and i = ref 0
	in	
		printList(ordList(mystring));
		while !i < (sizeStr - size(str)) do(	
		
			if (!i <= sizeStr) then (						
				print " 32"						
			)
			else();			
		i := !i + 1
		)
	end;
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
(*Для вызова в консоли нужно
1. load "Int";
2. Объявить функцию ordList
3. Объявить функцию printList
4. Объявить функцию main
5. Вызываем main параметром передаем строку)
*)