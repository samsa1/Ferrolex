{

open FerrolexParser

let word =
    let keywords = Hashtbl.create 12 in
    let words = [("as", AS); ("let", LET); ("parse", PARSE); "rule", RULE; "and", AND; "eof", EOF_WORD] in
    let () = List.iter (fun (s, t) -> Hashtbl.add keywords s t) words in
    let test x = 
        if Hashtbl.mem keywords x then Hashtbl.find keywords x else IDENT x
    in test
;;

let code_buffer = Buffer.create 512
;;

let string_buffer = Buffer.create 512
;;

}

let chiffre = ['0'-'9']
let integer = chiffre+
let alpha = ['a'-'z']|['A'-'Z']|'_'
let chaine = (alpha)(alpha | chiffre)*
let space = ' ' | '\t'
let car = [' '-'!']|['#'-'[']|[']'-'~'] 


rule token = parse
	| "{" {let _ = code lexbuf in let s = Buffer.contents code_buffer in Buffer.reset code_buffer; CODE s}
    | '-' { MINUS }
    | '+' { PLUS }
    | '=' { EQUAL }
    | '|' { VERT }
    | '(' { PARG }
    | ')' { PARD }
    | '[' { CRD }
    | ']' { CRG }
    | '*' { KLEENE }
    | integer as i { INT (int_of_string i) }
    | '\"'(['\t'-'!''#'-'~']* as s)'\"' { STRING s }
    | chaine as c { word c}
    | ';' { SEMICOLON }
    | space { token lexbuf }
    | '\n' { Lexing.new_line lexbuf; token lexbuf}
    | '\''(['\x00'-'\xff'] as c)'\'' { CHAR c}
    | "\'\\t\'" { CHAR '\t'}
    | "\'\\n\'" { CHAR '\n'}
    | ("\'\\\"\'" | '"') { CHAR '"'}
    | ("'\\\\'") { CHAR '\\' }
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

and code = parse
	| '}' { ()}
	| '{' {
		Buffer.add_char code_buffer '{';
		let _ = code lexbuf in
		Buffer.add_char code_buffer '}';
		code lexbuf
	}
    | '\"' { 
        Buffer.add_char code_buffer '"';
        string_parser lexbuf;
        Buffer.add_string code_buffer (Buffer.contents string_buffer);
        Buffer.reset string_buffer;
        Buffer.add_char code_buffer '"';
        code lexbuf
        } 
	| '\n' {Buffer.add_char code_buffer '\n'; Lexing.new_line lexbuf; code lexbuf}
	| _ as c {Buffer.add_char code_buffer c; code lexbuf}

and string_parser = parse 
    | '"' { () }
    | "\\\"" { Buffer.add_string string_buffer "\\\""; string_parser lexbuf}
    | _ as c { Buffer.add_char string_buffer c; string_parser lexbuf}
