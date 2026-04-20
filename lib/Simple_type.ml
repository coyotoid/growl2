type 'a var = {
  id : int;
  mutable upper : 'a list;
  mutable lower : 'a list;
  mutable level : int;
}
[@@deriving show]

type ty =
  | TError
  | TVar of ty var
  | TPrim of Type_primitive.t
  | TFunc of stack * stack

and stack = SError | SVar of stack var | SCons of ty * stack [@@deriving show]

module Infix = struct
  let ( &> ) s t = SCons (t, s)
  let ( => ) a b = TFunc (a, b)
end
