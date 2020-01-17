(* A typed embedding of Z3 in OCaml *)

Z3enums.sort_kind
Z3.Sort.k

type ('a, 'b) order = Lt | Eq : ('a, 'a) order | Gt

type 'a sort
type ('dom, 'cod) symbol
type 'a expr

(* Sort *)
module Sort : sig
  val name : 'a sort -> string
  val compare : 'a sort -> 'b sort -> ('a, 'b) order

  module type T = sig type t val sort : t sort end
end

(* Symbol *)
module Symbol : sig
  module Arity : sig
    type 'a params =
      | [] : unit params
      | (::) : 'a sort * 'b params -> ('a -> 'b) params
    type 'a args =
      | [] : unit args
      | (::) : 'a expr * 'b args -> ('a -> 'b) args
  end
  type 'a params = 'a Arity.params
  type 'a args = 'a Arity.args

  val fresh : ?name:string -> 'dom params -> 'cod sort -> ('dom, 'cod) symbol
  val dom : ('dom, 'cod) symbol -> 'dom params
  val cod : ('dom, 'cod) symbol -> 'cod sort
  val compare : ('dom, 'cod) symbol -> ('dom, 'cod) symbol -> int
end

(* Expression *)
module Expr : sig
  type 'a t = 'a expr

  type 'a desc =
    | Apply : ('dom, 'cod) symbol * 'dom Symbol.args -> 'cod desc
    | If_then_else of bool expr * 'a expr * 'a expr
    | Operator of 'a
    | Unexpected of string

  val apply : ('dom, 'cod) symbol -> 'dom Symbol.args -> 'cod expr
  val desc : 'a expr -> 'a desc
  val operator : 'a sort -> 'a -> 'a expr

  val equal : 'a expr -> 'a expr -> Bool.t expr
  val if_ : Bool.t expr -> then_:'a expr -> else_:'a expr -> 'a expr
end

module Bool : sig
  type t =
    | Literal of bool
    | Equal : 'a expr * 'a expr -> t
    | Distinct : 'a expr list -> t
    | Not of t expr
    | Iff of t expr * t expr
    | Implies of t expr * t expr
    | Xor of t expr * t expr
    | And of t expr list
    | Or of t expr list
  include Sort.T with type t := t

  val const : bool -> t expr
  val equal : 'a expr -> 'a expr -> t expr
  val distinct : 'a expr list -> t expr
  val not : t expr -> t expr
  val iff : t expr -> t expr -> t expr
  val implies : t expr -> t expr -> t expr
  val xor : t expr -> t expr -> t expr
  val and_ : t expr list -> t expr
  val or_ : t expr list -> t expr
end

module Context : sig
  type t
  val empty : t
  val assert_ : bool expr -> t -> t
  val check : t -> bool
end

(*type integer
type real

module type NUMERAL = sig
  include Sort.T
  val add : t expr -> t expr -> t expr
  val mul : t expr -> t expr -> t expr
  val sub : t expr -> t expr -> t expr
  val minus : t expr -> t expr
  val div : t expr -> t expr -> t expr
  val mod_ : t expr -> t expr -> t expr
  val rem : t expr -> t expr -> t expr
  val power : t expr -> t expr -> t expr
end

module Integer : sig
  include NUMERAL with type t = integer
  val to_real : t expr -> real expr
end

module Real : sig
  include NUMERAL with type t = real
  val to_integer : t expr -> integer expr
end

(* Bitvectors *)
module Bitvector : sig
  type 'a bv
  val sort : 'a bv -> 'a bv sort
  val not : 'a bv expr -> 'a bv expr
  val redand : 'a bv expr ->

end*)
