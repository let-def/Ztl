module Order : sig type ('a, 'b) t = Lt | Eq : ('a, 'a) t | Gt end
type ('a, 'b) order = ('a, 'b) Order.t

module Term : sig
  type 'a sort =
    | Boolean : boolean sort
    | Integer : integer sort
    | Real : real sort

  and 'a expr
  and ('dom, 'cod) symbol

  and boolean =
    | Boolean_literal of bool
    | Boolean_equal : 'a expr * 'a expr -> boolean
    | Boolean_distinct : 'a expr list -> boolean
    | Boolean_not of boolean expr
    | Boolean_iff of boolean expr * boolean expr
    | Boolean_implies of boolean expr * boolean expr
    | Boolean_xor of boolean expr * boolean expr
    | Boolean_and of boolean expr list
    | Boolean_or of boolean expr list
    | Boolean_is_integer of real expr
    | Boolean_le : 'a numeral expr * 'a numeral expr -> boolean
    | Boolean_lt : 'a numeral expr * 'a numeral expr -> boolean
    | Boolean_ge : 'a numeral expr * 'a numeral expr -> boolean
    | Boolean_gt : 'a numeral expr * 'a numeral expr -> boolean

  and 'a numeral =
    | Numeral_add      of 'a numeral expr list
    | Numeral_mul      of 'a numeral expr list
    | Numeral_sub      of 'a numeral expr list
    | Numeral_minus    of 'a numeral expr
    | Numeral_div      of 'a numeral expr * 'a numeral expr
    | Numeral_power    of 'a numeral expr * 'a numeral expr
    | Numeral_specific of 'a

  and integer = integer_ numeral
  and integer_ =
    | Integer_literal of string
    | Integer_from_real of real expr
    | Integer_modulus of integer expr * integer expr
    | Integer_remainder of integer expr * integer expr

  and real = real_ numeral
  and real_ =
    | Real_literal of string
    | Real_from_int of real expr
end

module Sort : sig
  val name : 'a Term.sort -> string
  val compare : 'a Term.sort -> 'b Term.sort -> ('a, 'b) order

  module type T = sig type t val sort : t Term.sort end
end

module Symbol : sig
  open Term

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
  (*val compare : ('dom, 'cod) symbol -> ('dom, 'cod) symbol -> int*)
end

module Expr : sig
  open Term

  type 'a desc =
    | Expr_apply : ('dom, 'cod) symbol * 'dom Symbol.args -> 'cod desc
    | Expr_if_then_else of boolean expr * 'a expr * 'a expr
    | Expr_operator of 'a
    | Expr_error of string

  val desc : 'a expr -> 'a desc
  val operator : 'a sort -> 'a -> 'a expr

  val apply : ('dom, 'cod) symbol -> 'dom Symbol.args -> 'cod expr
  val if_ : boolean expr -> then_:'a expr -> else_:'a expr -> 'a expr

  val of_bool : bool -> boolean expr
  val equal : 'a expr -> 'a expr -> boolean expr
  val distinct : 'a expr list -> boolean expr
  val not : boolean expr -> boolean expr
  val iff : boolean expr -> boolean expr -> boolean expr
  val implies : boolean expr -> boolean expr -> boolean expr
  val xor : boolean expr -> boolean expr -> boolean expr
  val and_ : boolean expr list -> boolean expr
  val or_ : boolean expr list -> boolean expr

  val add      : 'a numeral expr -> 'a numeral expr -> 'a numeral expr
  val multiply : 'a numeral expr -> 'a numeral expr -> 'a numeral expr
  val subtract : 'a numeral expr -> 'a numeral expr -> 'a numeral expr
  val negate   : 'a numeral expr -> 'a numeral expr
  val divide   : 'a numeral expr -> 'a numeral expr -> 'a numeral expr
  val power    : 'a numeral expr -> 'a numeral expr -> 'a numeral expr

  val lt : 'a numeral expr -> 'a numeral expr -> boolean expr
  val le : 'a numeral expr -> 'a numeral expr -> boolean expr
  val gt : 'a numeral expr -> 'a numeral expr -> boolean expr
  val ge : 'a numeral expr -> 'a numeral expr -> boolean expr

  val of_integer : string -> integer expr
  val integer_sum         : integer expr list -> integer expr
  val integer_subtraction : integer expr list -> integer expr
  val integer_product     : integer expr list -> integer expr
  val modulus : integer expr -> integer expr -> integer expr
  val remainder : integer expr -> integer expr -> integer expr

  val of_real : string -> real expr
  val real_sum         : real expr list -> real expr
  val real_subtraction : real expr list -> real expr
  val real_product     : real expr list -> real expr
end

module Infix : sig
  open Term

  val ( ==? )  : 'a expr -> 'a expr -> boolean expr
  val ( !? )   : boolean expr -> boolean expr
  val ( <=>?)  : boolean expr -> boolean expr -> boolean expr
  val ( ==>? ) : boolean expr -> boolean expr -> boolean expr
  val ( &&? )  : boolean expr -> boolean expr -> boolean expr
  val ( ||? )  : boolean expr -> boolean expr -> boolean expr

  val ( +? )  : 'a numeral expr -> 'a numeral expr -> 'a numeral expr
  val ( *? )  : 'a numeral expr -> 'a numeral expr -> 'a numeral expr
  val ( -? )  : 'a numeral expr -> 'a numeral expr -> 'a numeral expr
  val ( ~-? ) : 'a numeral expr -> 'a numeral expr
  val ( /? )  : 'a numeral expr -> 'a numeral expr -> 'a numeral expr
  val ( ^? )  : 'a numeral expr -> 'a numeral expr -> 'a numeral expr

  val ( % ) : ('dom, 'cod) symbol -> 'dom Symbol.args -> 'cod expr

  val ( <=? ) : 'a numeral expr -> 'a numeral expr -> boolean expr
  val ( <?  ) : 'a numeral expr -> 'a numeral expr -> boolean expr
  val ( >=? ) : 'a numeral expr -> 'a numeral expr -> boolean expr
  val ( >?  ) : 'a numeral expr -> 'a numeral expr -> boolean expr
end

module Context : sig
  open Term

  type t
  val empty : t
  val assert_ : boolean expr list -> t -> t
  val check : t -> boolean expr list -> [`Sat | `Unknown | `Unsat]
end
