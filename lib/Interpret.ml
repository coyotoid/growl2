(* TODO: make this use the query engine *)

open Containers
module String_map = Map.Make (String)

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VList of value list
  | VQuote of Ast.term * value String_map.t

let rec pp_value : value Fmt.t =
 fun ppf -> function
  | VInt i -> Fmt.int ppf i
  | VBool b -> Fmt.bool ppf b
  | VString s -> Fmt.quote (Fmt.of_to_string String.escaped) ppf s
  | VList l -> Fmt.braces (Fmt.list ~sep:Fmt.sp pp_value) ppf l
  | VQuote _ -> Fmt.string ppf "<quote>"

(* helpers *)
let binop_int stack op =
  match stack with
  | VInt b :: VInt a :: rest -> VInt (op a b) :: rest
  | _ :: _ :: rest -> failwith "type mismatch"
  | _ -> failwith "stack underflow"

let binop_cmp stack op =
  match stack with
  | VInt b :: VInt a :: rest -> VBool (op a b) :: rest
  | _ :: _ :: rest -> failwith "type mismatch"
  | _ -> failwith "stack underflow"

let binop_bool stack op =
  match stack with
  | VBool b :: VBool a :: rest -> VBool (op a b) :: rest
  | _ :: _ :: rest -> failwith "type mismatch"
  | _ -> failwith "stack underflow"

let unop_bool stack op =
  match stack with
  | VBool a :: rest -> VBool (op a) :: rest
  | _ :: _ :: rest -> failwith "type mismatch"
  | _ -> failwith "stack underflow"

(* term execution *)
let rec exec_term db lenv stack (term : Ast.term) =
  match term.value with
  | Id -> stack
  | Cat (f, g) ->
      let stack' = exec_term db lenv stack f in
      exec_term db lenv stack' g
  | Lit (`Int i) -> VInt i :: stack
  | Lit (`Bool b) -> VBool b :: stack
  | Lit (`String s) -> VString s :: stack
  | List ls ->
      let vals =
        List.map
          (fun el ->
            match exec_term db lenv [] el with [ v ] -> v | _ -> assert false)
          ls
      in
      VList vals :: stack
  | Word w -> exec_word db lenv stack w
  | Quote f -> VQuote (f, lenv) :: stack
  | Bind (name, body) -> (
      match stack with
      | [] -> failwith "stack underflow"
      | v :: rest ->
          let lenv' = String_map.add name.value v lenv in
          exec_term db lenv' rest body)
  | Command (name, body) ->
      let stack' = exec_term db lenv stack body in
      exec_word db lenv stack' name.value

and exec_word db lenv stack = function
  | "dup" -> (
      match stack with [] -> failwith "stack underflow" | v :: _ -> v :: stack)
  | "drop" -> (
      match stack with [] -> failwith "stack underflow" | _ :: rest -> rest)
  | "swap" -> (
      match stack with
      | a :: b :: rest -> b :: a :: rest
      | _ -> failwith "stack underflow")
  | "+" -> binop_int stack ( + )
  | "-" -> binop_int stack ( - )
  | "*" -> binop_int stack ( * )
  | "/" -> binop_int stack ( / )
  | "=" -> binop_cmp stack Int.( = )
  | "!=" -> binop_cmp stack Int.( <> )
  | "<" -> binop_cmp stack Int.( < )
  | ">" -> binop_cmp stack Int.( > )
  | "<=" -> binop_cmp stack Int.( < )
  | ">=" -> binop_cmp stack Int.( > )
  | "and" -> binop_bool stack Bool.( && )
  | "not" -> unop_bool stack Bool.(not)
  | "choose" -> (
      match stack with
      | q_f :: q_t :: VBool cond :: rest -> (if cond then q_t else q_f) :: rest
      | _ :: _ :: _ :: rest -> failwith "type mismatch"
      | _ -> failwith "stack underflow")
  | "call" -> (
      match stack with
      | VQuote (body, cap) :: rest -> exec_term db cap rest body
      | _ :: _ -> failwith "type mismatch"
      | _ -> failwith "stack underflow")
  | "dip" -> (
      match stack with
      | VQuote (body, cap) :: x :: rest ->
          let stack' = exec_term db cap rest body in
          x :: stack'
      | _ :: _ :: _ -> failwith "type mismatch"
      | _ -> failwith "stack underflow")
  | "list/singleton" -> (
      match stack with
      | a :: rest -> VList [ a ] :: rest
      | _ -> failwith "stack underflow")
  | "list/cons" -> (
      match stack with
      | a :: VList lst :: rest -> VList (a :: lst) :: rest
      | _ :: _ :: _ -> failwith "type mismatch"
      | _ -> failwith "stack underflow")
  | "list/uncons" -> (
      match stack with
      | VList [] :: rest -> failwith "uncons on empty list"
      | VList [ a ] :: rest -> VList [] :: a :: rest
      | VList lst :: rest -> VList (List.tl lst) :: List.hd lst :: rest
      | _ :: _ -> failwith "type mismatch"
      | _ -> failwith "stack underflow")
  | "list/empty?" -> (
      match stack with
      | VList [] :: rest -> VBool true :: rest
      | VList _ :: rest -> VBool false :: rest
      | _ :: _ -> failwith "type mismatch"
      | _ -> failwith "stack underflow")
  | "list/length" -> (
      match stack with
      | VList l :: rest -> VInt (List.length l) :: rest
      | _ :: _ -> failwith "type mismatch"
      | _ -> failwith "stack underflow")
  | w -> (
      match String_map.find_opt w lenv with
      | Some value -> value :: stack
      | None -> (
          let expr = Resolver.ask db (Query.WordExpr w) in
          match expr with
          | Some body -> exec_term db String_map.empty stack body
          | None -> failwith ("unbound word " ^ w)))

let exec db =
  let main_expr = Resolver.ask db (Query.WordExpr "main") in
  match main_expr with
  | Some main -> exec_term db String_map.empty [] main
  | _ -> failwith "no main function to execute"
