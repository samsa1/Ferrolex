
type ichar = char * int;; 

type regexpOld = 
	| EpsilonOld
	| CharacterOld of ichar
	| UnionOld of regexpOld * regexpOld
	| ConcatOld of regexpOld * regexpOld
	| StarOld of regexpOld;;

type charSet = 
  | Single of int
  | Segment of int * int
  | Rule
  | Except of charSet * charSet
  | Any
  | SetUnion of charSet * charSet
;;

type icharSet = charSet * int

type regexp = 
  | Epsilon
  | Character of icharSet
  | Union of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp
;;

module CsetOld = Set.Make(struct type t = ichar let compare = compare end);;
module Cset = Set.Make(struct type t = icharSet let compare = compare end);;

type state = 
  Cset.t
;;

module Cmap = Map.Make(Int);;
module Smap = Map.Make(Cset);;
module Imap = Map.Make(Int);;
module Iset = Set.Make(Int);;

type autom = {
	start : Cset.t;
	trans : state Cmap.t Smap.t;
  term : int option Smap.t
};;

type autom2 = {
	start2 : int;
	trans2 : int Cmap.t Imap.t;
  term2 : int option Imap.t
}


type file = {
  header : string;
  reg_list : (string * regexp * string) list;
  bottom : string;
}
