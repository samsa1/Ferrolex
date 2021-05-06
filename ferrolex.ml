
let file = ref ""
let lang = ref "ocaml"
let sortie = ref ""

let parse_only = ref false
let old_version = ref false

type lang = 
  |Rust
  |Ocaml

let decryptLang = function
  |"ocaml" | "ml" -> Ocaml
  |"rust" | "rs" -> Rust
  | _ -> failwith "unknown language"
let main () = 
  let spectlist = [
    "-l", Arg.Set_string lang, "set target language";
    "-o", Arg.Set_string sortie, "set output file name";
    "-min-char", Arg.Set_int Ferrolex_utilities.min_char, "set minimum char in iteration";
    "-max-char", Arg.Set_int Ferrolex_utilities.max_char, "set maximum char in iteration";
    "--parse-only", Arg.Set parse_only, "stop after parsing";
    "--old", Arg.Set old_version, "use old version with match (useful with high branching factor)";
    "-token-type", Arg.Set_string Ferrolex_utilities.token_type, "set token type name for languages needing it such as Rust"
  ]
  in
  Arg.parse spectlist (fun f -> file := f) "";
  if !file = "" then failwith "no file to compile was given";
  if not (Filename.check_suffix !file ".frl") then failwith "not the good extension";
  let language = decryptLang (String.lowercase_ascii !lang) in
  Ferrolex_var.default_error := (match language with
    |Ocaml -> "failwith \"End of file not implemented for this rule\""
    |Rust -> "Err(\"end of string not handled\")");
  let f = open_in !file in
  let buf = Lexing.from_channel f in
  let parsed = try FerrolexParser.file FerrolexLexer.token buf
    with FerrolexParser.Samenhir_Parsing_Error _ -> (let e = Lexing.lexeme_end_p buf in
    print_string "Parsing error line ";
    print_int e.pos_lnum;
    print_newline ();
    exit 1)
  in close_in f;
  if !parse_only then exit 0;
  match language with
    |Ocaml -> begin
      let outfile = if !sortie = "" then (Filename.chop_suffix !file ".frl" ^ ".ml") else !sortie in
      let out = open_out outfile in
      (if !old_version then Ferrolex_utilities.pp_ocaml_main_old else Ferrolex_utilities.pp_ocaml_main) (Format.formatter_of_out_channel out) parsed;
      close_out out
      end
    |Rust -> begin
      let outfile = if !sortie = "" then (Filename.chop_suffix !file ".frl" ^ ".rs") else !sortie in
      let out = open_out outfile in
      Ferrolex_utilities.pp_rust_main (Format.formatter_of_out_channel out) parsed;
      close_out out
      end
;;

main ();;
