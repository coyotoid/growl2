open Containers
module String_map = Map.Make (String)

type value =
  | VInt of int
  | VBool of bool
  | VQuote of Ast.term * value String_map.t

let pp_value : value Fmt.t =
 fun ppf -> function
  | VInt n -> Fmt.int ppf n
  | VBool n -> Fmt.bool ppf n
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

(* term execution *)
let rec exec_term wenv lenv stack (term : Ast.term) =
  match term.value with
  | Id -> stack
  | Cat (f, g) ->
      let stack' = exec_term wenv lenv stack f in
      exec_term wenv lenv stack' g
  | Lit (`Int n) -> VInt n :: stack
  | Lit (`Bool n) -> VBool n :: stack
  | Word w -> exec_word wenv lenv stack w
  | Quote f -> VQuote (f, lenv) :: stack
  | Bind (name, body) -> (
      match stack with
      | [] -> failwith "stack underflow"
      | v :: rest ->
          let lenv' = String_map.add name.value v lenv in
          exec_term wenv lenv' rest body)

and exec_word wenv lenv stack = function
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
  | "choose" -> (
      match stack with
      | q_f :: q_t :: VBool cond :: rest -> (if cond then q_t else q_f) :: rest
      | _ :: _ :: _ :: rest -> failwith "type mismatch"
      | _ -> failwith "stack underflow")
  | "call" -> (
      match stack with
      | VQuote (body, cap) :: rest -> exec_term wenv cap rest body
      | _ :: _ -> failwith "type mismatch"
      | _ -> failwith "stack underflow")
  | w -> (
      match String_map.find_opt w wenv with
      | Some body -> exec_term wenv String_map.empty stack body
      | None -> failwith ("unbound word: " ^ w))

let exec (prog : Ast.program) =
  (* Build word environment *)
  let word_env =
    List.fold_left
      (fun acc (def : Ast.def Span.Spanned.t) ->
        String_map.add def.value.name.value def.value.body acc)
      String_map.empty prog
  in
  match String_map.find_opt "main" word_env with
  | Some main -> exec_term word_env String_map.empty [] main
  | _ -> failwith "no main function to execute"
