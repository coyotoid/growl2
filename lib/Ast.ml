type literal = [ `Int of int | `Bool of bool | `String of string ]

type term' =
  | Id
  | Cat of term * term
  | Lit of literal
  | List of term list
  | Word of string
  | Quote of term
  | Bind of string Span.Spanned.t * term
  | Command of string Span.Spanned.t * term

and term = term' Span.Spanned.t

type def = {
  name : string Span.Spanned.t;
  annot : Type_ast.annot option;
  body : term;
}

type program = def Span.Spanned.t list

let primitive_of_literal : literal -> Type_primitive.t = function
  | `Int n when n >= 0 -> `Nat
  | `Int _ -> `Int
  | `Bool _ -> `Bool
  | `String _ -> `String

let seq terms =
  let spanned value = Span.(Spanned.{ value; span = dummy }) in
  Option.value ~default:Id
    (List.fold_right
       (fun x acc ->
         match acc with
         | None -> Some x
         | Some xs -> Some (Cat (spanned x, spanned xs)))
       terms None)
  |> spanned
