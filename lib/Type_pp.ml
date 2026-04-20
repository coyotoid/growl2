open Type

let rec pp_ty : ty Fmt.t =
 fun ppf t ->
  match t.node with
  | TError -> Fmt.string ppf "<error>"
  | TBot -> Fmt.string ppf "⊥"
  | TTop -> Fmt.string ppf "⊤"
  | TUnion (a, b) ->
      (Fmt.parens (Fmt.pair ~sep:(Fmt.any "@ | ") pp_ty pp_ty)) ppf (a, b)
  | TInter (a, b) ->
      (Fmt.parens (Fmt.pair ~sep:(Fmt.any "@ & ") pp_ty pp_ty)) ppf (a, b)
  | TFunc (a, b) ->
      (Fmt.parens (Fmt.pair ~sep:(Fmt.any "@ -> ") pp_stack pp_stack)) ppf (a, b)
  | TRec { name; body } -> Fmt.pf ppf "𝜇%s. %a" name pp_ty body
  | TVar v -> Fmt.(any "'" ++ string) ppf v
  | TPrim `Nat -> Fmt.string ppf "nat"
  | TPrim `Int -> Fmt.string ppf "int"
  | TPrim `Bool -> Fmt.string ppf "bool"

and pp_stack : stack Fmt.t =
 fun ppf s ->
  match s.node with
  | SError -> Fmt.string ppf "<error>"
  | SBot -> Fmt.string ppf "⊥"
  | STop -> Fmt.string ppf "⊤"
  | SUnion (a, b) ->
      (Fmt.parens (Fmt.pair ~sep:(Fmt.any "@ | ") pp_stack pp_stack)) ppf (a, b)
  | SInter (a, b) ->
      (Fmt.parens (Fmt.pair ~sep:(Fmt.any "@ & ") pp_stack pp_stack)) ppf (a, b)
  | SRec { name; body } -> Fmt.pf ppf "𝜇%s. %a" name pp_stack body
  | SVar v -> Fmt.(any "'" ++ string) ppf v
  | SCons (t, s) -> Fmt.pf ppf "%a, %a" pp_stack s pp_ty t
