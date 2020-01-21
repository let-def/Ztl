module IntSet = Set.Make(Int)

module StatefulIntSet : sig
  type state
  type operation = int

  val make : unit -> state
  val get : state -> IntSet.t

  val push : state -> unit
  val pop : state -> int -> unit
  val apply : state -> operation list -> unit
end = struct
  type state = IntSet.t list ref
  type operation = int

  let make () = ref []

  let get states = match !states with
    | [] -> IntSet.empty
    | x :: _ -> x

  let push states =
    states := get states :: !states

  let pop states n =
    let rec aux = function
      | (0, xs) -> xs
      | (n, (_ :: xs)) -> aux (n - 1, xs)
      | (_, []) -> assert false
    in
    states := aux (n, !states)

  let apply states ops =
    match !states with
    | [] -> assert false
    | x :: xs -> states := IntSet.add_seq (List.to_seq ops) x :: xs
end

module Tracker = Ztl__State_tracker.Make(StatefulIntSet)

let () = Random.self_init ()

let success = ref 0
let errors = ref 0

let rec test state n =
  if n = 0 then (
    let (pure, tracked) = state in
    Tracker.setup tracked;
    let purified = StatefulIntSet.get (Tracker.state tracked) in
    if not (IntSet.equal pure purified) then (
      prerr_endline "Reference and tracked differ:";
      let pr_set set =
        set
        |> IntSet.to_seq
        |> Seq.map string_of_int |> List.of_seq |> String.concat ";"
      in
      prerr_endline ("reference: " ^ pr_set pure);
      prerr_endline ("tracked:   " ^ pr_set purified);
      incr errors
    )
    else
      incr success;
    []
  ) else (
    match Random.int 10 with
    | 1 -> test state (n - 1) @ test state (n - 1)
    | 2 -> [state, n]
    | 3 -> retry (test state (n - 1))
    | _ ->
      let r = Random.int 100 in
      let (pure, tracked) = state in
      let state = (IntSet.add r pure, Tracker.apply r tracked) in
      test state (n - 1)
  )

and retry tests =
  List.flatten (List.map (fun (st, n) -> test st n) tests)

let rec retry_all = function
  | [] -> ()
  | xs -> retry_all (retry xs)

let () = retry_all (test (IntSet.empty, Tracker.make (StatefulIntSet.make ())) 60)

let () =
  Printf.printf "%d success / %d failures\n" !success !errors

let () = if !errors > 0 then exit 1
