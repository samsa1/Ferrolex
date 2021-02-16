
val min_char : int ref
val max_char : int ref

val pp_ocaml_main : Format.formatter -> string * (string *FerrolexAst.regexp) list * string -> unit

val pp_ocaml_main_old : Format.formatter -> string * (string *FerrolexAst.regexp) list * string -> unit

val main : string * (string *FerrolexAst.regexp) list * string -> unit