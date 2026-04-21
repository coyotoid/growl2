open Containers

type ty_node =
  | TError
  | TTop
  | TBot
  | TUnion of ty * ty
  | TInter of ty * ty
  | TFunc of stack * stack
  | TRec of { name : string; body : ty }
  | TVar of string
  | TPrim of Type_primitive.t
  | TCon of string * ty list

and ty = ty_node Hashcons.hash_consed

and stack_node =
  | SError
  | STop
  | SBot
  | SUnion of stack * stack
  | SInter of stack * stack
  | SRec of { name : string; body : stack }
  | SVar of string
  | SCons of ty * stack

and stack = stack_node Hashcons.hash_consed

module Hc_ty = Hashcons.Make (struct
  type t = ty_node

  let equal a b =
    match (a, b) with
    | TUnion (a1, a2), TUnion (b1, b2) | TInter (a1, a2), TInter (b1, b2) ->
        a1.tag = b1.tag && a2.tag = b2.tag
    | TFunc (a1, a2), TFunc (b1, b2) -> a1.tag = b1.tag && a2.tag = b2.tag
    | TRec r1, TRec r2 ->
        String.equal r1.name r2.name && r1.body.tag = r2.body.tag
    | TVar a, TVar b -> String.equal a b
    | TPrim a, TPrim b -> Type_primitive.equal a b
    | TCon (c1, args1), TCon (c2, args2) ->
        String.equal c1 c2
        && List.for_all2 (fun (a : ty) (b : ty) -> a.tag = b.tag) args1 args2
    | TError, TError | TTop, TTop | TBot, TBot -> true
    | _ -> false

  let hash = function
    | TUnion (a, b) -> Hashtbl.hash (0, a.hkey, b.hkey)
    | TInter (a, b) -> Hashtbl.hash (1, a.hkey, b.hkey)
    | TFunc (a, b) -> Hashtbl.hash (2, a.hkey, b.hkey)
    | TRec { name; body } -> Hashtbl.hash (3, name, body.hkey)
    | TVar s -> Hashtbl.hash (4, s)
    | TPrim p -> Hashtbl.hash (5, p)
    | TCon (c, args) ->
        Hashtbl.hash (6, c, List.map (fun (a : ty) -> a.hkey) args)
    | TError -> Hashtbl.hash 7
    | TTop -> Hashtbl.hash 8
    | TBot -> Hashtbl.hash 9
end)

module Hc_stack = Hashcons.Make (struct
  type t = stack_node

  let equal a b =
    match (a, b) with
    | SUnion (a1, a2), SUnion (b1, b2) | SInter (a1, a2), SInter (b1, b2) ->
        a1.tag = b1.tag && a2.tag = b2.tag
    | SCons (t1, s1), SCons (t2, s2) -> t1.tag = t2.tag && s1.tag = s2.tag
    | SRec r1, SRec r2 ->
        String.equal r1.name r2.name && r1.body.tag = r2.body.tag
    | SVar a, SVar b -> String.equal a b
    | SError, SError | STop, STop | SBot, SBot -> true
    | _ -> false

  let hash = function
    | SUnion (a, b) -> Hashtbl.hash (0, a.hkey, b.hkey)
    | SInter (a, b) -> Hashtbl.hash (1, a.hkey, b.hkey)
    | SCons (t, s) -> Hashtbl.hash (2, t.hkey, s.hkey)
    | SRec { name; body } -> Hashtbl.hash (3, name, body.hkey)
    | SVar s -> Hashtbl.hash (4, s)
    | SError -> Hashtbl.hash 5
    | STop -> Hashtbl.hash 6
    | SBot -> Hashtbl.hash 7
end)

let ty_table = Hc_ty.create 64
let stk_table = Hc_stack.create 64

(* smart constructors *)
let terror : ty = Hc_ty.hashcons ty_table TError
let ttop : ty = Hc_ty.hashcons ty_table TTop
let tbot : ty = Hc_ty.hashcons ty_table TBot
let tunion : ty -> ty -> ty = fun a b -> Hc_ty.hashcons ty_table (TUnion (a, b))
let tinter : ty -> ty -> ty = fun a b -> Hc_ty.hashcons ty_table (TInter (a, b))

let tfunc : stack -> stack -> ty =
 fun a b -> Hc_ty.hashcons ty_table (TFunc (a, b))

let tvar : string -> ty = fun n -> Hc_ty.hashcons ty_table (TVar n)

let trec : string -> ty -> ty =
 fun n bd -> Hc_ty.hashcons ty_table (TRec { name = n; body = bd })

let tprim : Type_primitive.t -> ty = fun p -> Hc_ty.hashcons ty_table (TPrim p)

let tcon : string -> ty list -> ty =
 fun c args -> Hc_ty.hashcons ty_table (TCon (c, args))

let serror : stack = Hc_stack.hashcons stk_table SError
let stop : stack = Hc_stack.hashcons stk_table STop
let sbot : stack = Hc_stack.hashcons stk_table SBot

let sunion : stack -> stack -> stack =
 fun a b -> Hc_stack.hashcons stk_table (SUnion (a, b))

let sinter : stack -> stack -> stack =
 fun a b -> Hc_stack.hashcons stk_table (SInter (a, b))

let scons : ty -> stack -> stack =
 fun a b -> Hc_stack.hashcons stk_table (SCons (a, b))

let svar : string -> stack = fun n -> Hc_stack.hashcons stk_table (SVar n)

let srec : string -> stack -> stack =
 fun n bd -> Hc_stack.hashcons stk_table (SRec { name = n; body = bd })

(* type simplification *)
let rec simplify_ty (t : ty) : ty =
  match t.node with
  | TInter (a, b) -> (
      let a = simplify_ty a and b = simplify_ty b in
      match (a.node, b.node) with
      | TTop, _ -> b
      | _, TTop -> a
      | _ when a.tag = b.tag -> a
      | TPrim p, TPrim q -> (
          match Type_primitive.meet p q with
          | Some r -> tprim r
          | None -> tinter a b)
      | _ -> tinter a b)
  | TUnion (a, b) -> (
      let a = simplify_ty a and b = simplify_ty b in
      match (a.node, b.node) with
      | TBot, _ -> b
      | _, TBot -> a
      | _ when a.tag = b.tag -> a
      | TPrim p, TPrim q -> (
          match Type_primitive.join p q with
          | Some r -> tprim r
          | None -> tunion a b)
      | _ -> tunion a b)
  | TFunc (a, b) -> tfunc (simplify_stack a) (simplify_stack b)
  | TCon (c, args) -> tcon c (List.map simplify_ty args)
  | TRec { name; body } -> trec name (simplify_ty body)
  | _ -> t

and simplify_stack (s : stack) : stack =
  match s.node with
  | SInter (a, b) -> (
      let a = simplify_stack a and b = simplify_stack b in
      match (a.node, b.node) with
      | STop, _ -> b
      | _, STop -> a
      | _ when a.tag = b.tag -> a
      | _ -> sinter a b)
  | SUnion (a, b) -> (
      let a = simplify_stack a and b = simplify_stack b in
      match (a.node, b.node) with
      | SBot, _ -> b
      | _, SBot -> a
      | _ when a.tag = b.tag -> a
      | _ -> sunion a b)
  | SCons (t, s) -> scons (simplify_ty t) (simplify_stack s)
  | SRec { name; body } -> srec name (simplify_stack body)
  | _ -> s
