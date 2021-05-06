
val min_char : int ref
val max_char : int ref
val token_type : string ref

val pp_ocaml_main : Format.formatter -> FerrolexAst.file -> unit

val pp_ocaml_main_old : Format.formatter -> FerrolexAst.file -> unit

val pp_rust_main : Format.formatter -> FerrolexAst.file -> unit