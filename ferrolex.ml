
let file = ref ""

let main () = 
  let spectlist = []
  in
  Arg.parse spectlist (fun f -> file := f) "";
  if !file = "" then failwith "no file to compile was given";
  if not (Filename.check_suffix !file ".frl") then failwith "not the good extension";
  let f = open_in !file in
  let buf = Lexing.from_channel f in
  let parsed = try FerrolexParser.file FerrolexLexer.token buf
    with FerrolexParser.Samenhir_Parsing_Error _ -> (let e = Lexing.lexeme_end_p buf in
    print_string "Parsing error line ";
    print_int e.pos_lnum;
    print_newline ();
    exit 1)
  in close_in f;
  print_string "parsed!\n";
  let outfile = (Filename.chop_suffix !file ".sam" ^ ".ml") in
  let out = open_out outfile in
  Ferrolex_utilities.pp_ocaml_main (Format.formatter_of_out_channel out) parsed;
  close_out out
;;

main ();;