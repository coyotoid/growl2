type ty =
  | TPrim of Type_primitive.t
  | TFunc of arrow
  | TCon of string * ty list
  | TVar of string

and stack_item = SItem of ty | SRest of string
and arrow = { inputs : stack_item list; outputs : stack_item list }

type annot = arrow

let ty_of_string = function
  | "nat" -> TPrim `Nat
  | "int" -> TPrim `Int
  | "bool" -> TPrim `Bool
  | "string" -> TPrim `String
  | a -> TVar a
