type t = [ `Int | `Nat | `Bool ] [@@deriving eq, ord, show]

let leq : t -> t -> bool =
 fun p1 p2 ->
  match (p1, p2) with
  | `Nat, `Int -> true
  | p, q when equal p q -> true
  | _ -> false

let join : t -> t -> t option =
 fun p1 p2 ->
  match (p1, p2) with
  | `Nat, `Int | `Int, `Nat -> Some `Int
  | p, q when equal p q -> Some p
  | _ -> None

let meet : t -> t -> t option =
 fun p1 p2 ->
  match (p1, p2) with
  | `Nat, `Int | `Int, `Nat -> Some `Nat
  | p, q when equal p q -> Some p
  | _ -> None
