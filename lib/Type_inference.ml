open Containers
module String_map = Map.Make (String)

type ctx = {
  env : Simple_type.ty String_map.t;
  ask : 'a. 'a Query.t -> 'a;
  placeholders : Simple_type.ty String_map.t;
}

module type S = sig
  val next_id : unit -> int
  val fresh_ty_var : level:int -> unit -> Simple_type.ty
  val fresh_stack_var : level:int -> unit -> Simple_type.stack
  val infer : ctx -> int -> Ast.term -> Simple_type.ty
  val constrain_ty : Simple_type.ty -> Simple_type.ty -> bool
end

module Make () : S = struct
  let id = ref 0
  let next_id () = Ref.get_then_incr id

  let fresh_var ~level () =
    Simple_type.{ id = next_id (); upper = []; lower = []; level }

  let fresh_ty_var ~level () = Simple_type.TVar (fresh_var ~level ())
  let fresh_stack_var ~level () = Simple_type.SVar (fresh_var ~level ())

  let coalesce t =
    let module C = Type_coalescing.Make () in
    t |> Compact_type.compact |> Compact_type.simplify |> C.coalesce
    |> Type.simplify_ty

  let rec constrain_ty lhs rhs : bool =
    let open Simple_type in
    match (lhs, rhs) with
    | TError, _ | _, TError -> false
    | TVar a, _ ->
        if not (List.memq rhs a.upper) then (
          a.upper <- rhs :: a.upper;
          List.fold_left (fun acc lb -> acc && constrain_ty lb rhs) true a.lower)
        else true
    | _, TVar b ->
        if not (List.memq lhs b.lower) then (
          b.lower <- lhs :: b.lower;
          List.fold_left (fun acc ub -> acc && constrain_ty lhs ub) true b.upper)
        else true
    | TPrim p, TPrim q ->
        if not (Type_primitive.leq p q) then
          let () =
            Diagnosed.throw `Error
              Text.
                [
                  Any (p, Type_primitive.pp);
                  Text " is not a subtype of ";
                  Any (q, Type_primitive.pp);
                ]
          in
          false
        else true
    | TFunc (s1, s2), TFunc (s3, s4) ->
        constrain_stack s3 s1 && constrain_stack s2 s4
    | TCon (c1, args1), TCon (c2, args2) when String.equal c1 c2 ->
        let rec go l1 l2 =
          match (l1, l2) with
          | [], [] -> true
          | a :: t1, b :: t2 ->
              let r = constrain_ty a b in
              let rest = go t1 t2 in
              r && rest
          | _ -> false
        in
        go args1 args2
    | _ ->
        let () =
          Diagnosed.throw `Error
            Text.
              [
                Text "type mismatch between ";
                Any (coalesce lhs, Type_pp.pp_ty);
                Text " and ";
                Any (coalesce rhs, Type_pp.pp_ty);
              ]
        in
        false

  and constrain_stack lhs rhs : bool =
    let open Simple_type in
    match (lhs, rhs) with
    | SError, _ | _, SError -> false
    | SVar a, _ ->
        if not (List.memq rhs a.upper) then (
          a.upper <- rhs :: a.upper;
          List.fold_left
            (fun acc lb -> acc && constrain_stack lb rhs)
            true a.lower)
        else true
    | _, SVar b ->
        if not (List.memq lhs b.lower) then (
          b.lower <- lhs :: b.lower;
          List.fold_left
            (fun acc ub -> acc && constrain_stack lhs ub)
            true b.upper)
        else true
    | SCons (t1, s1), SCons (t2, s2) ->
        constrain_ty t1 t2 && constrain_stack s1 s2

  let freshen level =
    let open Simple_type in
    let ty_cache = Hashtbl.create 4 in
    let stk_cache = Hashtbl.create 4 in
    let rec go_ty = function
      | TError -> TError
      | TPrim _ as t -> t
      | TFunc (lhs, rhs) -> TFunc (go_stack lhs, go_stack rhs)
      | TCon (name, args) -> TCon (name, List.map go_ty args)
      | TVar v -> (
          match Hashtbl.find_opt ty_cache v.id with
          | Some v' -> TVar v'
          | None ->
              let v' = fresh_var ~level () in
              Hashtbl.add ty_cache v.id v';
              v'.lower <- List.map go_ty v.lower;
              v'.upper <- List.map go_ty v.upper;
              TVar v')
    and go_stack = function
      | SError -> SError
      | SCons (t, s) -> SCons (go_ty t, go_stack s)
      | SVar v -> (
          match Hashtbl.find_opt stk_cache v.id with
          | Some v' -> SVar v'
          | None ->
              let v' = fresh_var ~level () in
              Hashtbl.add stk_cache v.id v';
              v'.lower <- List.map go_stack v.lower;
              v'.upper <- List.map go_stack v.upper;
              SVar v')
    in
    go_ty

  let type_of_word ~span ctx level name =
    let open Simple_type in
    match String_map.find_opt name ctx.env with
    | Some ty ->
        let rho = fresh_stack_var ~level () in
        TFunc (rho, SCons (ty, rho))
    | None -> (
        match String_map.find_opt name ctx.placeholders with
        | Some ph -> ph
        | None ->
            let t =
              Diagnosed.adorn ~span (fun () -> ctx.ask (Query.WordType name))
            in
            freshen level t)

  let rec infer ctx level (term : Ast.term) : Simple_type.ty =
    let open Simple_type in
    let open Ast in
    match term.value with
    | Id ->
        let rho = fresh_stack_var ~level () in
        TFunc (rho, rho)
    | Lit l ->
        let rho = fresh_stack_var ~level () in
        TFunc (rho, SCons (TPrim (primitive_of_literal l), rho))
    | List ts ->
        let elem_ty = fresh_ty_var ~level () in
        List.iter
          (fun el ->
            match infer ctx level el with
            | TFunc (s_in, SCons (t, _)) -> ignore (constrain_ty t elem_ty)
            | _ -> assert false)
          ts;
        let rho = fresh_stack_var ~level () in
        TFunc (rho, SCons (TCon ("list", [ elem_ty ]), rho))
    | Word name ->
        Diagnosed.adorn ~span:term.span (fun () ->
            type_of_word ~span:term.span ctx level name)
    | Cat (f, g) -> (
        let tf = Diagnosed.adorn ~span:f.span (fun () -> infer ctx level f) in
        let tg = Diagnosed.adorn ~span:g.span (fun () -> infer ctx level g) in
        match (tf, tg) with
        | TFunc (s_in, s_mid), TFunc (s_mid', s_out) ->
            if
              Diagnosed.adorn ~span:term.span (fun () ->
                  constrain_stack s_mid s_mid')
            then TFunc (s_in, s_out)
            else TFunc (SError, SError)
        | _ -> assert false)
    | Quote f ->
        let tf = Diagnosed.adorn ~span:f.span (fun () -> infer ctx level f) in
        let rho = fresh_stack_var ~level () in
        TFunc (rho, SCons (tf, rho))
    | Bind (name, body) -> (
        let t = fresh_ty_var ~level () in
        let rho = fresh_stack_var ~level () in
        let body_ty =
          infer
            { ctx with env = String_map.add name.value t ctx.env }
            level body
        in
        match body_ty with
        | TFunc (body_in, body_out) ->
            let ok =
              Diagnosed.adorn ~span:term.span (fun () ->
                  constrain_stack rho body_in)
            in
            if ok then TFunc (SCons (t, rho), body_out)
            else TFunc (SError, SError)
        | _ -> assert false)
    | Command (name, body) -> (
        let body_ty =
          Diagnosed.adorn ~span:body.span (fun () -> infer ctx level body)
        in
        let word_ty =
          Diagnosed.adorn ~span:name.span (fun () ->
              type_of_word ~span:name.span ctx level name.value)
        in
        match (body_ty, word_ty) with
        | TFunc (s_in, s_mid), TFunc (s_mid', s_out) ->
            let result =
              Diagnosed.adorn ~span:(Span.merge name.span body.span) (fun () ->
                  constrain_stack s_mid s_mid')
            in
            if result then TFunc (s_in, s_out) else TFunc (SError, SError)
        | _ -> assert false)
end
