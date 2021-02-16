{

open FerrolexParser

let debut = ref true


let word =
    let keywords = Hashtbl.create 12 in
    let words = [("as", AS); ("let", LET); ("parse", PARSE); "rule", RULE; "and", AND] in
    let () = List.iter (fun (s, t) -> Hashtbl.add keywords s t) words in
    let test x = 
        if Hashtbl.mem keywords x then Hashtbl.find keywords x else IDENT x
    in test

}

let chiffre = ['0'-'9']
let integer = chiffre+
let alpha = ['a'-'z']|['A'-'Z']|'_'
let chaine = (alpha)(alpha | chiffre)*
let chaine = (alpha)(alpha | chiffre)*
let chaineCode = [^'}']
let space = ' ' | '\t'
let car = [' '-'!']|['#'-'[']|[']'-'~'] 


rule token2 = parse
    | '{' (chaineCode* as c) '}' { CODE c }
    | '-' { MINUS }
    | '+' { PLUS }
    | '=' { EQUAL }
    | '|' { VERT }
    | '(' { PARD }
    | ')' { PARG }
    | '[' { CRD }
    | ']' { CRG }
    | '*' { KLEENE }
    | integer as i { INT (int_of_string i) }
    | '\"'(['\t'-'!''#'-'~']* as s)'\"' { STRING s }
    | chaine+ as c { word c}
    | space { token lexbuf }
    | '\n' {Lexing.new_line lexbuf; token lexbuf}
    | '\''(['\x00'-'\xff'] as c)'\'' { CHAR c}
    | "\'\\t\'" { CHAR '\t'}
    | "\'\\n\'" { CHAR '\n'}
    | "\'\\\"\'" { CHAR '"'}
    | _ as c {
        let p = Lexing.lexeme_start_p lexbuf in
        print_int p.pos_lnum;
        print_char ' ';
        print_int p.pos_cnum;
        print_string "\nUnknown char :\'";
        print_char c;
        print_string "\'\n";
        exit 1;
        }
    | eof {EOF}
and token = parse
    | '{' (chaineCode* as c) '}' { CODE c }
    | '-' { MINUS }
    | '+' { PLUS }
    | '=' { EQUAL }
    | '|' { VERT }
    | '(' { PARD }
    | ')' { PARG }
    | '[' { CRD }
    | ']' { CRG }
    | '*' { KLEENE }
    | integer as i { INT (int_of_string i) }
    | '\"'(['\t'-'!''#'-'~']* as s)'\"' { STRING s }
    | chaine+ as c { word c}
    | space { token lexbuf }
    | '\n' {Lexing.new_line lexbuf; token lexbuf}
    | '\''(['\x00'-'\xff'] as c)'\'' { CHAR c}
    | "\'\\t\'" { CHAR '\t'}
    | "\'\\n\'" { CHAR '\n'}
    | "\'\\\"\'" { CHAR '"'}
    | "\'\\x"(chiffre chiffre as c)'\'' { CHAR (char_of_int (int_of_string c))}
    | '_' {ANY}
    | '^' {EXCEPT}
    | _ as c {
        let p = Lexing.lexeme_start_p lexbuf in
        print_int p.pos_lnum;
        print_char ' ';
        print_int p.pos_cnum;
        print_string "\nUnknown char :\'";
        print_char c;
        print_string "\'\n";
        exit 1;
        }
    | eof {EOF}
