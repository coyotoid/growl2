type 'a var = {
  id : int;
  mutable upper : 'a list;
  mutable lower : 'a list;
  mutable level : int;
}

type ty =
  | TError
  | TVar of ty var
  | TPrim of Type_primitive.t
  | TFunc of stack * stack

and stack = SError | SVar of stack var | SCons of ty * stack

module Infix = struct
  let ( &> ) s t = SCons (t, s)
  let ( => ) a b = TFunc (a, b)
end

let is_error =
  let rec go_ty = function
    | TError -> true
    | TVar _ -> false
    | TPrim _ -> false
    | TFunc (a, b) -> go_stack a || go_stack b
  and go_stack = function
    | SError -> true
    | SVar _ -> false
    | SCons (t, s) -> go_ty t || go_stack s
  in
  go_ty
