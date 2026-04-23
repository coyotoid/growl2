module S = Span.Spanned

type literal = [ `Int of int | `Bool of bool | `String of string ]

type term' =
  | Id
  | Cat of term * term
  | Lit of literal
  | List of term list
  | Word of string
  | Quote of term
  | Bind of string S.t * term
  | Command of string S.t * term

and term = term' S.t

type def = { name : string S.t; annot : Type_ast.annot S.t option; body : term }
type program = def S.t list

let primitive_of_literal : literal -> Type_primitive.t = function
  | `Int n when n >= 0 -> `Nat
  | `Int _ -> `Int
  | `Bool _ -> `Bool
  | `String _ -> `String
