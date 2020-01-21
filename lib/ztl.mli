open Strong
type 'a natural = 'a Natural.t

type signedness = [ `Signed | `Unsigned ]

type boolean   = private Sort_boolean
type _ numeral = private Sort_numeral
type integer_  = private Sort_integer
type integer   = integer_ numeral
type real_     = private Sort_real
type real      = real_ numeral
type (_, _) zarray = private Sort_zarray
type _ zset     = private Sort_zset
type _ bitvector = private Sort_bitvector

type 'a sort =
  | Boolean : boolean sort
  | Integer : integer sort
  | Real : real sort
  | Zarray : 'a sort * 'b sort -> ('a, 'b) zarray sort
  | Zset : 'a sort -> 'a zset sort
  | Bitvector : 'a natural -> 'a bitvector sort

type 'a term
type ('dom, 'cod) symbol

module Sort : sig
  val name : 'a sort -> string
  val order : 'a sort -> 'b sort -> ('a, 'b) order
end

module Symbol : sig
  module Arity : sig
    type 'a params =
      | [] : unit params
      | (::) : 'a sort * 'b params -> ('a -> 'b) params
    type 'a args =
      | [] : unit args
      | (::) : 'a term * 'b args -> ('a -> 'b) args
    type ('i, 'a) array_args =
      | [] : ('i, unit) array_args
      | (::) : ('i, 'a) zarray term * ('i, 'b) array_args ->
          ('i, 'a -> 'b) array_args
  end

  type 'a params = 'a Arity.params
  type 'a args = 'a Arity.args
  type ('i, 'd) array_args = ('i, 'd) Arity.array_args

  val fresh :
    ?name:string -> 'dom params -> 'cod sort -> ('dom, 'cod) symbol
  val const :
    ?name:string -> 'cod sort -> (unit, 'cod) symbol
  val dom : ('dom, 'cod) symbol -> 'dom params
  val cod : ('dom, 'cod) symbol -> 'cod sort
end

module Term : sig
  type 'a desc =
    | Apply : ('dom, 'cod) symbol * 'dom Symbol.args -> 'cod desc
    | If_then_else : boolean term * 'a term * 'a term -> 'a desc

    | Boolean_literal  : bool -> boolean desc
    | Boolean_equal    : 'a term * 'a term -> boolean desc
    | Boolean_distinct : 'a term list -> boolean desc
    | Boolean_not      : boolean term -> boolean desc
    | Boolean_iff      : boolean term * boolean term -> boolean desc
    | Boolean_implies  : boolean term * boolean term -> boolean desc
    | Boolean_xor      : boolean term * boolean term -> boolean desc
    | Boolean_and      : boolean term list -> boolean desc
    | Boolean_or       : boolean term list -> boolean desc

    | Numeral_add   : 'a numeral term list -> 'a numeral desc
    | Numeral_mul   : 'a numeral term list -> 'a numeral desc
    | Numeral_sub   : 'a numeral term list -> 'a numeral desc
    | Numeral_minus : 'a numeral term -> 'a numeral desc
    | Numeral_div   : 'a numeral term * 'a numeral term -> 'a numeral desc
    | Numeral_power : 'a numeral term * 'a numeral term -> 'a numeral desc
    | Numeral_le    : 'a numeral term * 'a numeral term -> boolean desc
    | Numeral_lt    : 'a numeral term * 'a numeral term -> boolean desc
    | Numeral_ge    : 'a numeral term * 'a numeral term -> boolean desc
    | Numeral_gt    : 'a numeral term * 'a numeral term -> boolean desc

    | Integer_literal : string -> integer desc
    | Integer_of_real : real term -> integer desc
    | Integer_modulus : integer term * integer term -> integer desc
    | Integer_remainder : integer term * integer term -> integer desc

    | Real_literal    : string -> real desc
    | Real_of_integer : integer term -> real desc
    | Real_is_integer : real term -> boolean desc

    | Zarray_const  : 'a sort * 'b term -> ('a, 'b) zarray desc
    | Zarray_select : ('a, 'b) zarray term * 'a term -> 'b desc
    | Zarray_store  : ('a, 'b) zarray term * 'a term * 'b term -> ('a, 'b) zarray desc
    | Zarray_map : ('a1 -> 'a2, 'b) symbol * ('i, 'a1 -> 'a2) Symbol.array_args -> ('i, 'b) zarray desc

    | Zset_empty  : 'a sort -> 'a zset desc
    | Zset_full   : 'a sort -> 'a zset desc
    | Zset_add    : 'a term * 'a zset term -> 'a zset desc
    | Zset_remove : 'a term * 'a zset term -> 'a zset desc
    | Zset_union  : 'a zset term * 'a zset term -> 'a zset desc
    | Zset_intersect : 'a zset term * 'a zset term -> 'a zset desc
    | Zset_difference : 'a zset term * 'a zset term -> 'a zset desc
    | Zset_complement : 'a zset term -> 'a zset desc
    | Zset_is_member : 'a term * 'a zset term -> boolean desc
    | Zset_is_subset : 'a zset term * 'a zset term -> boolean desc

    | Bitvector_not : 'n bitvector term -> 'n bitvector desc
    | Bitvector_redand : 'n bitvector term -> Natural.one bitvector desc
    | Bitvector_redor : 'n bitvector term -> Natural.one bitvector desc
    | Bitvector_and  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_or   : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_xor  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_nand : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_nor  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_xnor : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_neg  : 'n bitvector term -> 'n bitvector desc
    | Bitvector_add  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_sub  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_mul  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_udiv : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_sdiv : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_urem : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_srem : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_smod : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_ult  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_slt  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_ule  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_sle  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_uge  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_sge  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_ugt  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_sgt  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_shl  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_lshr : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_ashr : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_extend : signedness * 'm natural * 'n bitvector term -> 'm bitvector desc
    | Bitvector_rotate_left  : 'n bitvector term * int -> 'n bitvector desc
    | Bitvector_rotate_right : 'n bitvector term * int -> 'n bitvector desc
    | Bitvector_ext_rotate_left  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_ext_rotate_right : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_of_integer : 'n natural * integer term -> 'n bitvector desc
    | Bitvector_to_integer : signedness * 'n bitvector term -> integer desc
    | Bitvector_add_no_overflow  : 'n bitvector term * 'n bitvector term * signedness -> 'n bitvector desc
    | Bitvector_add_no_underflow : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_sub_no_overflow  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_sub_no_underflow : 'n bitvector term * 'n bitvector term * signedness -> 'n bitvector desc
    | Bitvector_sdiv_no_overflow : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_neg_no_overflow  : 'n bitvector term -> 'n bitvector desc
    | Bitvector_mul_no_overflow  : 'n bitvector term * 'n bitvector term * signedness -> 'n bitvector desc
    | Bitvector_mul_no_underflow : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_numeral : 'n natural * string -> 'n bitvector desc
  (*| Bitvector_concat   : context -> Expr.term -> Expr.term -> Expr.term*)
  (*| Bitvector_extract  : context -> int -> int -> Expr.term -> Expr.term*)
  (*| Bitvector_repeat   : context -> int -> Expr.term -> Expr.term*)

  val desc : 'a term -> 'a desc
  val make : 'a sort -> 'a desc -> 'a term

  val apply : ('dom, 'cod) symbol -> 'dom Symbol.args -> 'cod term
  val const : (unit, 'cod) symbol -> 'cod term
  val if_ : boolean term -> then_:'a term -> else_:'a term -> 'a term
end

module Boolean : sig
  val of_bool : bool -> boolean term
  val equal : 'a term -> 'a term -> boolean term
  val distinct : 'a term list -> boolean term
  val not : boolean term -> boolean term
  val iff : boolean term -> boolean term -> boolean term
  val implies : boolean term -> boolean term -> boolean term
  val xor : boolean term -> boolean term -> boolean term
  val and_ : boolean term list -> boolean term
  val or_ : boolean term list -> boolean term
end

module Numeral : sig
  val add      : 'a numeral term -> 'a numeral term -> 'a numeral term
  val multiply : 'a numeral term -> 'a numeral term -> 'a numeral term
  val subtract : 'a numeral term -> 'a numeral term -> 'a numeral term
  val negate   : 'a numeral term -> 'a numeral term
  val divide   : 'a numeral term -> 'a numeral term -> 'a numeral term
  val power    : 'a numeral term -> 'a numeral term -> 'a numeral term

  val lt : 'a numeral term -> 'a numeral term -> boolean term
  val le : 'a numeral term -> 'a numeral term -> boolean term
  val gt : 'a numeral term -> 'a numeral term -> boolean term
  val ge : 'a numeral term -> 'a numeral term -> boolean term
end

module Integer : sig
  val of_string : string -> integer term
  val of_int : int -> integer term
  val of_real : real term -> integer term

  val sum : integer term list -> integer term
  val subtract : integer term list -> integer term
  val product : integer term list -> integer term
  val modulus : integer term -> integer term -> integer term
  val remainder : integer term -> integer term -> integer term
end

module Real : sig
  val of_string : string -> real term
  val of_integer : integer term -> real term

  val sum : real term list -> real term
  val subtract : real term list -> real term
  val product : real term list -> real term

  val is_integer : real term -> boolean term
end

module Zarray : sig
  val constant : 'a sort -> 'b term -> ('a, 'b) zarray term
  val select : ('a, 'b) zarray term -> 'a term -> 'b term
  val store  : ('a, 'b) zarray term -> 'a term -> 'b term -> ('a, 'b) zarray term
  val map : ('a1->'a2, 'b) symbol -> ('i, 'a1->'a2) Symbol.array_args -> ('i, 'b) zarray term
end

module Zset : sig
  val empty  : 'a sort -> 'a zset term
  val full   : 'a sort -> 'a zset term
  val add    : 'a term -> 'a zset term -> 'a zset term
  val remove : 'a term -> 'a zset term -> 'a zset term
  val union  : 'a zset term -> 'a zset term -> 'a zset term
  val intersect : 'a zset term -> 'a zset term -> 'a zset term
  val difference : 'a zset term -> 'a zset term -> 'a zset term
  val complement : 'a zset term -> 'a zset term
  val is_member : 'a term -> 'a zset term -> boolean term
  val is_subset : 'a zset term -> 'a zset term -> boolean term
end

module Bitvector : sig
  type 'n bv = 'n bitvector term
  val not  : 'n bv -> 'n bv
  val and_ : 'n bv -> 'n bv -> 'n bv
  val or_  : 'n bv -> 'n bv -> 'n bv
  val xor  : 'n bv -> 'n bv -> 'n bv
  val nand : 'n bv -> 'n bv -> 'n bv
  val nor  : 'n bv -> 'n bv -> 'n bv
  val xnor : 'n bv -> 'n bv -> 'n bv
  val neg  : 'n bv -> 'n bv
  val add  : 'n bv -> 'n bv -> 'n bv
  val sub  : 'n bv -> 'n bv -> 'n bv
  val mul  : 'n bv -> 'n bv -> 'n bv
  val udiv : 'n bv -> 'n bv -> 'n bv
  val sdiv : 'n bv -> 'n bv -> 'n bv
  val urem : 'n bv -> 'n bv -> 'n bv
  val srem : 'n bv -> 'n bv -> 'n bv
  val smod : 'n bv -> 'n bv -> 'n bv
  val ult  : 'n bv -> 'n bv -> boolean term
  val slt  : 'n bv -> 'n bv -> boolean term
  val ule  : 'n bv -> 'n bv -> boolean term
  val sle  : 'n bv -> 'n bv -> boolean term
  val uge  : 'n bv -> 'n bv -> boolean term
  val sge  : 'n bv -> 'n bv -> boolean term
  val ugt  : 'n bv -> 'n bv -> boolean term
  val sgt  : 'n bv -> 'n bv -> boolean term
  val shl  : 'n bv -> 'n bv -> 'n bv
  val lshr : 'n bv -> 'n bv -> 'n bv
  val ashr : 'n bv -> 'n bv -> 'n bv
  val redand : 'n bv -> Natural.one bv
  val redor  : 'n bv -> Natural.one bv
  val extend : signedness -> 'm natural -> 'n bv -> 'm bv
  val rotate_left  : 'n bv -> int -> 'n bv
  val rotate_right : 'n bv -> int -> 'n bv
  val rotate_left'  : 'n bv -> 'n bv -> 'n bv
  val rotate_right' : 'n bv -> 'n bv -> 'n bv
  val of_integer : 'n natural -> integer term -> 'n bv
  val to_integer : signedness -> 'n bv -> integer term
  val add_no_overflow  : 'n bv -> 'n bv -> signedness -> 'n bv
  val add_no_underflow : 'n bv -> 'n bv -> 'n bv
  val sub_no_overflow  : 'n bv -> 'n bv -> 'n bv
  val sub_no_underflow : 'n bv -> 'n bv -> signedness -> 'n bv
  val sdiv_no_overflow : 'n bv -> 'n bv -> 'n bv
  val neg_no_overflow  : 'n bv -> 'n bv
  val mul_no_overflow  : 'n bv -> 'n bv -> signedness -> 'n bv
  val mul_no_underflow : 'n bv -> 'n bv -> 'n bv
  val numeral : 'n natural -> string -> 'n bv
end

module Infix : sig
  val ( ==? )  : 'a term -> 'a term -> boolean term
  val ( <>? )  : 'a term -> 'a term -> boolean term
  (*val ( !? ) : boolean term -> boolean term*)
  val ( <=>?)  : boolean term -> boolean term -> boolean term
  val ( ==>? ) : boolean term -> boolean term -> boolean term
  val ( &&? )  : boolean term -> boolean term -> boolean term
  val ( ||? )  : boolean term -> boolean term -> boolean term

  val ( +? )  : 'a numeral term -> 'a numeral term -> 'a numeral term
  val ( *? )  : 'a numeral term -> 'a numeral term -> 'a numeral term
  val ( -? )  : 'a numeral term -> 'a numeral term -> 'a numeral term
  (*val ( ~-? ) : 'a numeral term -> 'a numeral term*)
  val ( /? )  : 'a numeral term -> 'a numeral term -> 'a numeral term
  val ( ^? )  : 'a numeral term -> 'a numeral term -> 'a numeral term

  val ( % ) : ('dom, 'cod) symbol -> 'dom Symbol.args -> 'cod term

  val ( <=? ) : 'a numeral term -> 'a numeral term -> boolean term
  val ( <?  ) : 'a numeral term -> 'a numeral term -> boolean term
  val ( >=? ) : 'a numeral term -> 'a numeral term -> boolean term
  val ( >?  ) : 'a numeral term -> 'a numeral term -> boolean term
end

module Context : sig
  type t
  val empty : t
  val assert_ : boolean term list -> t -> t
  val check :
    ?context:t -> boolean term list ->
    [`Possible | `Unknown | `Impossible]
end

(* Low-level Z3 interface *)
val z3_context   : Z3.context
val z3_sort      : _ sort -> Z3.Sort.sort
val z3_expr      : _ term -> Z3.Expr.expr
val z3_symbol    : _ symbol -> Z3.Symbol.symbol
val z3_func_decl : _ symbol -> Z3.FuncDecl.func_decl
