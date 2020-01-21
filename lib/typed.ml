type (_, _) eq = Refl : ('a, 'a) eq
let follow_eq (type a b) (Refl : (a, b) eq) (x : a) : b = x

module Order = struct type (_, _) t = Lt | Eq : ('a, 'a) t | Gt end
type ('a, 'b) order = ('a, 'b) Order.t

module Natural = struct
  type 'a t = T : int -> unit t

  let order (type a b) (T a : a t) (T b : b t) : (a, b) order =
    Order.(if a < b then Lt else if a > b then Gt else Eq)

  let lift_eq (type a b) (Refl : (a, b) eq) : (a t, b t) eq =
    Refl

  let to_int (type n) (T n : n t) = n

  type zero = unit
  let zero : zero t = T 0

  type one = unit
  let one : one t = T 1

  module type T = sig type n val n : n t end

  module Nth (N : sig val n : int end) : T = struct
    type n = unit let n : n t = T N.n
  end

  let nth n =
    let module N = struct
      type n = unit
      let n = T n
    end
    in
    (module N : T)

  type ('a, 'b) sum = unit
  let add (type a b) (T a : a t) (T b : b t) : (a, b) sum t =
    T (a + b)
  let sum_comm (type a b)
    : ((a, b) sum, (b, a) sum) eq = Refl
  let sum_assoc (type a b c)
    : (((a, b) sum, c) sum, (a, (b, c) sum) sum) eq = Refl

  type ('a, 'b) prod = unit
  let mul (type a b) (T a : a t) (T b : b t) : (a, b) prod t =
    T (a * b)
  let prod_comm (type a b)
    : ((a, b) prod, (b, a) prod) eq = Refl
  let prod_assoc (type a b c)
    : (((a, b) prod, c) prod, (a, (b, c) prod) prod) eq = Refl
end

type 'a natural = 'a Natural.t
