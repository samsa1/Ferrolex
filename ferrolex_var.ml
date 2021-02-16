open FerrolexAst

let hashCode: ((int, string) Hashtbl.t) = Hashtbl.create 64

let hashNames: ((int, string option) Hashtbl.t) = Hashtbl.create 64

let num = ref 0
let new_char (c:char) = 
  let i = !num in incr num;
  Character (Single (int_of_char c), i)
;;
let new_chars i1 = 
  let i = !num in incr num;
  Character (i1, i)
;; 

let new_rule () = 
  let i = !num in incr num;
  i
;;

let rec clean = function
  |Epsilon -> Epsilon
  |Character (Rule, i) -> Character (Rule, i)
  |Character (c, _) -> Character (c, new_rule ())
  |Star r -> Star (clean r)
  |Union (r1, r2) -> Union (clean r1, clean r2)
  |Concat (r1, r2) -> Concat (clean r1, clean r2)
;;

let char_set i1 i2 = 
  if i1 > i2 then begin
    print_string "YOU ARE  NOT ALLOWED TO DO THAT YOU M****CKER";
    print_newline ();
    end;
  let sortie = ref (new_char (char_of_int i2)) in
  for i = i1 to i2 do
    sortie := Union (!sortie, new_char (char_of_int i))
  done;
  !sortie