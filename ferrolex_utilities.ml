
open FerrolexAst

let max_char = ref 255;;
let min_char = ref 0;;
let token_type = ref "parser::Token";;

(* pretty printer of regular expression *)
let rec pp_regexp_car = function
  | Rule -> assert false
  | Segment (i1, i2) -> begin
    print_char (char_of_int i1);
    print_char '-';
    print_char (char_of_int i2);
    end
  | Single c -> print_char (char_of_int c)
  | Any -> print_char '_'
  | Except (r1, r2) -> begin
      pp_regexp_car r1;
      print_char '^';
      pp_regexp_car r2;
  end
  | SetUnion (r1, r2) -> begin pp_regexp_car r1; pp_regexp_car r2 end
;;

let rec pp_regexp = function
    | Epsilon -> ()
    | Character (Rule , i) -> print_string ("{"^string_of_int i^"}")
    | Character (Single c, _) -> print_char (char_of_int c)
    | Character (c, _) -> begin
      print_char '[';
      pp_regexp_car c;
      print_char ']';
    end
    | Star r -> begin
        print_char '(';
        pp_regexp r;
        print_string ")*";
      end
    | Concat (r1, r2) -> begin
        pp_regexp r1;
        pp_regexp r2;
      end
    | Union (r1, r2) -> begin 
        print_char '(';
        pp_regexp r1;
        print_char '|';
        pp_regexp r2;
        print_char ')';
    end
;;

let rec pp_file = function 
    |[] -> ()
    |(s, r)::tl -> begin
        print_string s;
        print_string " : \n";
        pp_regexp r;
        print_newline ();
        pp_file tl;
      end
;;



let rec null = function 
	| Epsilon -> true 
	| Character _ -> false
	| Star _ -> true
	| Union (a,b) -> null a || null b 
	| Concat (a,b) -> null a && null b;;

let rec first = function 
  | Epsilon -> Cset.empty
  | Character a -> Cset.singleton a
  | Star s -> first s 
  | Union (a,b) -> Cset.union (first a) (first b)
  | Concat (a,b) -> if null a then Cset.union (first a) (first b) else first a;;

let rec last = function 
  | Epsilon -> Cset.empty
  | Character a -> Cset.singleton a
  | Star s -> last s 
  | Union (a,b) -> Cset.union (last a) (last b)
  | Concat (a,b) -> if null b then Cset.union (last a) (last b) else last b;;

let rec follow chr  = function
	| Epsilon -> Cset.empty
	| Character _ -> Cset.empty
	| Star s -> let fin = last s in if Cset.mem chr fin then 
			Cset.union (follow chr s) (first s)
		else follow chr s 
	| Concat (a,b) -> let fin = last a in
		if Cset.mem chr fin then 
			Cset.union (follow chr a) (Cset.union (first b) (follow chr b))
		else Cset.union (follow chr a) (follow chr b)
	| Union (a,b) -> Cset.union (follow chr a) (follow chr b)

let rec is_in chr = function
  | Single i -> i = chr
  | Segment (i, j) -> i <= chr && chr <= j
  | Rule -> false
  | Any -> true
  | Except (r1, r2) -> is_in chr r1 && not (is_in chr r2)
  | SetUnion (r1, r2) -> (is_in chr r1) || (is_in chr r2)
;;

let next_state reg cset chr =
  Cset.fold (fun (c,i) cset2 -> if is_in chr c then Cset.union cset2 (follow (c,i) reg) else cset2) cset Cset.empty
;;

let make_dfa rule = 
  let trans = ref Smap.empty in 
  let term = ref Smap.empty in 
  let rec transitions q = 
    if Smap.mem q !trans then () else
    begin
      	trans := Smap.add q Cmap.empty !trans;
      	let t = ref Cmap.empty in
      	for c = !min_char to !max_char do 
        	let suite = next_state rule q c in
        	if not (Cset.is_empty suite) then
        	begin
	        	t := Cmap.add c suite !t;
    	    	transitions suite;
    	    end;
      	done;
     	let final = Cset.fold
      		(fun (c, i) v ->
	      		if c <> Rule
	      		then v
	      		else
	      			match v with
	      				| None -> Some i
	      				| Some j -> Some (min i j): icharSet -> int option -> int option)
      		q
      		None
  		in
      	term := Smap.add q final !term;
      	trans := Smap.add q !t !trans;
     end
  in 
  let q0 = first rule in  transitions q0;
  {start = q0; trans = !trans; term = !term};;

let convert autom = 
    let num = ref 0 in 
    let valeurs = ref Smap.empty in 
    Smap.iter (fun k i -> (valeurs := Smap.add k !num !valeurs; num := 1 + !num)) autom.trans;
    let transitions = ref Imap.empty in 
    let aux m = 
      let t = ref Cmap.empty in 
      Cmap.iter (fun c s -> t := Cmap.add c (match s with s -> Smap.find s !valeurs) !t) m;
      !t
    in Smap.iter (fun k m -> transitions := Imap.add (Smap.find k !valeurs) (aux m) !transitions) autom.trans; 
    let term = Smap.fold (fun t opt m -> Imap.add (Smap.find t !valeurs) opt m) autom.term Imap.empty in
    Smap.iter (fun k i -> print_int i; print_char ' '; print_int (Cset.cardinal k); print_newline ()) !valeurs;
    {start2 = Smap.find autom.start !valeurs; trans2 = !transitions; term2 = term}
;;

let diminue_taille m =
  let deb = ref (-1) in
  let fin = ref (-1) in
  let sortie = ref [] in
  let aux s = 
  if !fin = -1 then begin
    deb := s;
    fin := s
    end
  else if !fin + 1 = s then fin := s
  else begin
    sortie := (!deb, !fin):: !sortie;
    deb := s;
    fin := s;
    end
  in  Iset.iter aux m;
  if !deb = -1 then [] else (!deb, !fin)::!sortie

let optimiseTrans trans = 
  let m = Cmap.fold (fun key next m -> if Imap.mem next m then Imap.add next (Iset.add key (Imap.find next m)) m else Imap.add next (Iset.singleton key) m) trans Imap.empty
  in Cmap.map diminue_taille m
;;

let createAutom r = 
  convert (make_dfa r)
;;

(***************************************************)
(*                                                 *)
(*                   OCAML CODE                    *)
(*                                                 *)
(***************************************************)

let isFirst = ref true
;;
let pp_ocaml_header fmt =
  Format.fprintf fmt "
type lexbuf = {
  mutable pos_b_lnum : int;
  mutable pos_b_cnum : int;
  mutable pos_e_cnum : int;
  mutable current : int;
  mutable read : int;
  text : string;
};;

let new_line buf = 
  buf.pos_b_lnum <- buf.pos_b_lnum + 1
;;

let incrPos buf = 
  buf.current <- buf.current + 1;
  buf
;;

exception Nothing

let newLexbuf text = {
  pos_b_lnum = 1;
  pos_b_cnum = 0;
  pos_e_cnum = 0;
  current = 0;
  read = 0;
  text = text
}

";;

let pp_ocaml_transition fmt name next liste =
  Format.fprintf fmt "\tif ";
  let aux1 fmt (d, f) =
      if d = f then Format.fprintf fmt "chr = %i" d else Format.fprintf fmt "(%i <= chr && chr <= %i)" d f
  in let aux pos = if pos = 0 then Format.fprintf fmt "%a" aux1 else Format.fprintf fmt "||%a" aux1 in
  List.iteri aux liste;
  Format.fprintf fmt " then %s_%i (incrPos lexbuf) else\n" name next;
;;

let pp_ocaml_autom_state fmt name (term: int option Imap.t) i transition = 
  Format.fprintf fmt "and %s_%i lexbuf = " name i;
  begin match Imap.find i term with 
    |None -> ()
    |Some j -> Format.fprintf fmt "\n\tlexbuf.read <- %i;\n\tlexbuf.pos_e_cnum <- lexbuf.current;\n\t" j
  end;
  Format.fprintf fmt "let chr = int_of_char lexbuf.text.[lexbuf.current] in\n";
  let minimized_trans = optimiseTrans transition in
  Cmap.iter (pp_ocaml_transition fmt name) minimized_trans;
  Format.fprintf fmt "\traise Nothing\n"
;;

let pp_ocaml_regexp fmt (s, reg, eof_code) = 
  let is_first = !isFirst in
  isFirst := false;
  let rec chercheBornes = function
    | Epsilon -> Iset.empty
    | Character (Rule, i) -> Iset.singleton i
    | Character _ -> Iset.empty
    | Star r -> chercheBornes r
    | Concat (r1, r2) | Union (r1, r2) -> Iset.union (chercheBornes r1) (chercheBornes r2)
  in let autom = try createAutom reg with Not_found -> assert false in
  Format.fprintf fmt "%s %s lexbuf = \n" (if is_first then "let rec" else "and") s;
  Format.fprintf fmt "\tif lexbuf.current = String.length lexbuf.text then %s else begin\n\tlexbuf.pos_b_cnum <- lexbuf.pos_e_cnum;\n\ttry %s_%i lexbuf\n with
  | _ -> begin
    let i = lexbuf.read in
    lexbuf.read <- 0;
    lexbuf.current <- lexbuf.pos_e_cnum;
    if lexbuf.pos_e_cnum <= lexbuf.pos_b_cnum then failwith \"Empty token\"
" eof_code s autom.start2;
  let aux i = 
      let code = try Hashtbl.find Ferrolex_var.hashCode i with Not_found -> pp_regexp reg; print_string "\n"; print_string " "; print_int i; print_newline (); assert false in
      let var = try Hashtbl.find Ferrolex_var.hashNames i with Not_found -> assert false in
      match var with
        | None -> Format.fprintf fmt "\t\telse if i = %i then begin %s end\n" i code
        | Some name -> Format.fprintf fmt "\t\telse if i = %i then let %s = String.sub lexbuf.text lexbuf.pos_b_cnum (lexbuf.pos_e_cnum - lexbuf.pos_b_cnum) in begin %s end\n" i name code
  in Iset.iter aux (chercheBornes reg);
  Format.fprintf fmt "\t\telse assert false\n\t\tend end\n";
  Imap.iter (pp_ocaml_autom_state fmt s autom.term2) autom.trans2;
;;

let pp_ocaml_main fmt file = 
  pp_ocaml_header fmt;
  Format.fprintf fmt "%s\n" file.header;
  (try List.iter (pp_ocaml_regexp fmt) file.reg_list with Not_found -> assert false);
  Format.fprintf fmt "\n%s\n" file.bottom;
  Format.pp_print_flush fmt ()
;;


(***************************************************)
(*                                                 *)
(*                   OCAML CODE                    *)
(*                   OLD VERSION                   *)
(*                                                 *)
(***************************************************)

let pp_ocaml_transition_old fmt name character next =
  Format.fprintf fmt "\t\t|%i -> %s_%i (incrPos lexbuf)\n" character name next
;;

let pp_ocaml_autom_state_old fmt name (term: int option Imap.t) i transition = 
  Format.fprintf fmt "and %s_%i lexbuf = " name i;
  begin match Imap.find i term with 
    |None -> ()
    |Some j -> Format.fprintf fmt "\n\tlexbuf.read <- %i;\n\tlexbuf.pos_e_cnum <- lexbuf.current;\n\t" j
  end;
  (*Format.fprintf fmt "lexbuf.current <- lexbuf.current + 1;\n\t";*)
  Format.fprintf fmt "match int_of_char lexbuf.text.[lexbuf.current] with\n";
  Cmap.iter (pp_ocaml_transition_old fmt name) transition;
  Format.fprintf fmt "\t\t|_ -> raise Nothing\n"
;;

let pp_ocaml_regexp_old fmt (s, reg , eof_code) = 
  let is_first = !isFirst in
  isFirst := false;
  let rec chercheBornes = function
    | Epsilon -> Iset.empty
    | Character (Rule, i) -> Iset.singleton i
    | Character _ -> Iset.empty
    | Star r -> chercheBornes r
    | Concat (r1, r2) | Union (r1, r2) -> Iset.union (chercheBornes r1) (chercheBornes r2)
  in let autom = try createAutom reg with Not_found -> assert false in
  Format.fprintf fmt "%s %s lexbuf = \n" (if is_first then "let rec" else "and") s;
  Format.fprintf fmt "\tif lexbuf.current = String.length lexbuf.text then %s else begin\n\tlexbuf.pos_b_cnum <- lexbuf.pos_e_cnum;\n\ttry %s_%i lexbuf\n with
  | _ -> begin
    let i = lexbuf.read in
    lexbuf.read <- 0;
    lexbuf.current <- lexbuf.pos_e_cnum;
    if lexbuf.pos_e_cnum <= lexbuf.pos_b_cnum then failwith \"Empty token\"
" eof_code s autom.start2;
  let aux i = 
      let code = try Hashtbl.find Ferrolex_var.hashCode i with Not_found -> pp_regexp reg; print_string "\n"; print_string " "; print_int i; print_newline (); assert false in
      let var = try Hashtbl.find Ferrolex_var.hashNames i with Not_found -> assert false in
      match var with
        | None -> Format.fprintf fmt "\t\telse if i = %i then begin %s end\n" i code
        | Some name -> Format.fprintf fmt "\t\telse if i = %i then let %s = String.sub lexbuf.text lexbuf.pos_b_cnum (lexbuf.pos_e_cnum - lexbuf.pos_b_cnum) in begin %s end\n" i name code
  in Iset.iter aux (chercheBornes reg);
  Format.fprintf fmt "\t\telse assert false\n\t\tend end\n";
  Imap.iter (pp_ocaml_autom_state_old fmt s autom.term2) autom.trans2;
;;

let pp_ocaml_main_old fmt file = 
  pp_ocaml_header fmt;
  Format.fprintf fmt "%s\n" file.header;
  (try List.iter (pp_ocaml_regexp_old fmt) file.reg_list with Not_found -> assert false);
  Format.fprintf fmt "\n%s\n" file.bottom;
  Format.pp_print_flush fmt ()
;;


(***************************************************)
(*                                                 *)
(*                    RUST CODE                    *)
(*                                                 *)
(***************************************************)

let pp_rust_header fmt =
  Format.fprintf fmt "
use alloc::string::String;

pub struct Lexbuf {
  pos_b_lnum : usize,
  pos_b_cnum : usize,
  pos_e_cnum : usize,
  current : usize,
  read : usize,
  text : String,
  len : usize,
}

impl Lexbuf {
  fn newline(&mut self) {
    self.pos_b_lnum = self.pos_b_lnum + 1;
  }

  fn incr_pos(&mut self) -> &mut Self {
    self.current = self.current + 1;
    self
  }

  pub fn new(text : String) -> Self {
    Lexbuf {
      pos_b_lnum : 1,
      pos_b_cnum : 0,
      pos_e_cnum : 0,
      current : 0,
      read : 0,
      len : text.len(),
      text : text,
    }
  }

  fn length(&self) -> usize {
    self.len
  }

  fn reset_current(&mut self) { self.current = self.pos_e_cnum; }

  fn get_read(&mut self) -> usize { self.read }
  fn set_read(&mut self, i : usize) {
    self.pos_e_cnum = self.current;
    self.read = i;
  }


  fn next_char(&mut self) -> Result<u8, u8> {
    if self.len <= self.current {
      Err(0)
    } else {
      Ok(self.text.chars().nth(self.current).unwrap() as u8)
    }
  }


  fn finished(&mut self) -> bool {
    self.len == self.current
  }

  fn new_extremity(&mut self) {
    self.pos_b_cnum = self.pos_e_cnum
  }

  fn get_token(&self) -> String {
    let mut s = String::new();
    for i in self.pos_b_cnum..self.pos_e_cnum {
      s.push(self.text.chars().nth(i).unwrap());
    }
    s
  }

}

";;

let pp_rust_transition fmt name next liste =
  Format.fprintf fmt "\tif ";
  let aux1 fmt (d, f) =
      if d = f then Format.fprintf fmt "chr == %i" d else 
      	if d > 0 then Format.fprintf fmt "(%i <= chr && chr <= %i)" d f
      	else Format.fprintf fmt "chr <= %i" f
  in let aux pos = if pos = 0 then Format.fprintf fmt "%a" aux1 else Format.fprintf fmt "||%a" aux1 in
  List.iteri aux liste;
  Format.fprintf fmt " { %s_%i(lexbuf.incr_pos()) } else\n" name next;
;;

let pp_rust_autom_state fmt name (term: int option Imap.t) i transition = 
  Format.fprintf fmt "fn %s_%i(lexbuf : &mut Lexbuf) -> Result<(),()> { " name i;
  begin match Imap.find i term with 
    |None -> ()
    |Some j -> Format.fprintf fmt "\n\tlexbuf.set_read(%i);\n\t" j
  end;
  Format.fprintf fmt "if let Ok(chr) = lexbuf.next_char() {\n";
  let minimized_trans = optimiseTrans transition in
  Cmap.iter (pp_rust_transition fmt name) minimized_trans;
  Format.fprintf fmt "\t{ Err(()) }\n\t} else {Err(())}\n}\n"
;;

let pp_rust_regexp fmt (s, reg, eof_code) = 
  let rec chercheBornes = function
    | Epsilon -> Iset.empty
    | Character (Rule, i) -> Iset.singleton i
    | Character _ -> Iset.empty
    | Star r -> chercheBornes r
    | Concat (r1, r2) | Union (r1, r2) -> Iset.union (chercheBornes r1) (chercheBornes r2)
  in let autom = try createAutom reg with Not_found -> assert false in
  Format.fprintf fmt "pub fn %s (lexbuf: &mut Lexbuf) -> Result<%s, &'static str> { \n" s !token_type;
  Format.fprintf fmt "\tif lexbuf.finished() {\n\t\t %s \n\t} else {
    lexbuf.new_extremity();
    match %s_%i(lexbuf) {
      _ => {
        let i = lexbuf.get_read();
        lexbuf.reset_current();
        lexbuf.set_read(0);
        if lexbuf.pos_e_cnum <= lexbuf.pos_b_cnum { Err(\"Reached End of File without any corrresponding rule\") }
" eof_code s autom.start2;
  let aux i = 
      let code = try Hashtbl.find Ferrolex_var.hashCode i with Not_found -> pp_regexp reg; print_string "\n"; print_string " "; print_int i; print_newline (); assert false in
      let var = try Hashtbl.find Ferrolex_var.hashNames i with Not_found -> assert false in
      match var with
        | None -> Format.fprintf fmt "\t\telse if i == %i { %s }\n" i code
        | Some name -> Format.fprintf fmt "\t\telse if i == %i { let %s = lexbuf.get_token(); %s }\n" i name code
  in Iset.iter aux (chercheBornes reg);
  Format.fprintf fmt "\t\telse { Err(\"Undefined rule, should not happen, please report this\")\n\t\t}\n\t\t}\n\t}\n\t}\n}\n";
  Imap.iter (pp_rust_autom_state fmt s autom.term2) autom.trans2;
;;

let pp_rust_main fmt file = 
  pp_rust_header fmt;
  Format.fprintf fmt "%s\n" file.header;
  (try List.iter (pp_rust_regexp fmt) file.reg_list with Not_found -> assert false);
  Format.fprintf fmt "\n%s\n\n" file.bottom;
  Format.pp_print_flush fmt ()

