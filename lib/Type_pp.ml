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
  | TRec { name; body } ->
      Fmt.pair ~sep:Fmt.sp
        Fmt.(any "μ" ++ string ++ any ".")
        pp_ty ppf (name, body)
  | TVar v -> Fmt.string ppf v
  | TPrim `Nat -> Fmt.string ppf "nat"
  | TPrim `Int -> Fmt.string ppf "int"
  | TPrim `Bool -> Fmt.string ppf "bool"
  | TPrim `String -> Fmt.string ppf "string"
  | TCon (name, []) -> Fmt.string ppf name
  | TCon (name, [ arg ]) -> Fmt.pair ~sep:Fmt.sp pp_ty Fmt.string ppf (arg, name)
  | TCon (name, args) ->
      Fmt.pair ~sep:Fmt.sp
        (Fmt.parens (Fmt.list ~sep:Fmt.comma pp_ty))
        Fmt.string ppf (args, name)

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
  | SRec { name; body } ->
      Fmt.pair ~sep:Fmt.sp
        Fmt.(any "μ" ++ string ++ any ".")
        pp_stack ppf (name, body)
  | SVar v -> Fmt.(any ".." ++ string) ppf v
  | SCons (t, s) -> Fmt.pair ~sep:Fmt.comma pp_stack pp_ty ppf (s, t)

let show_ty = Fmt.to_to_string pp_ty
let show_stack = Fmt.to_to_string pp_stack
