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
  val infer : ctx -> int -> Ast.term -> Simple_type.ty Diagnosed.t
  val constrain_ty : Simple_type.ty -> Simple_type.ty -> bool Diagnosed.t
end

module Make () : S = struct
  let id = ref 0
  let next_id () = Ref.get_then_incr id

  let fresh_var ~level () =
    Simple_type.{ id = next_id (); upper = []; lower = []; level }

  let fresh_ty_var ~level () = Simple_type.TVar (fresh_var ~level ())
  let fresh_stack_var ~level () = Simple_type.SVar (fresh_var ~level ())

  let coalesce t =
    let module C = Coalescing.Make () in
    t |> Compact_type.compact |> Compact_type.simplify |> C.coalesce
    |> Type.simplify_ty

  let rec constrain_ty lhs rhs : bool Diagnosed.t =
    let open Diagnosed in
    let open Simple_type in
    match (lhs, rhs) with
    | TVar a, _ ->
        if not (List.memq rhs a.upper) then (
          a.upper <- rhs :: a.upper;
          List.fold_left
            (fun acc lb ->
              let* acc = acc in
              let* res = constrain_ty lb rhs in
              return (acc && res))
            (return true) a.lower)
        else return true
    | _, TVar b ->
        if not (List.memq lhs b.lower) then (
          b.lower <- lhs :: b.lower;
          List.fold_left
            (fun acc ub ->
              let* acc = acc in
              let* res = constrain_ty lhs ub in
              return (acc && res))
            (return true) b.upper)
        else return true
    | TPrim p, TPrim q ->
        if not (Type_primitive.leq p q) then
          let+ () =
            throw `Error
              Text.
                [
                  Any (p, Type_primitive.pp);
                  Text " is not a subtype of ";
                  Any (q, Type_primitive.pp);
                ]
          in
          false
        else return true
    | TFunc (s1, s2), TFunc (s3, s4) ->
        let* r1 = constrain_stack s3 s1 in
        let* r2 = constrain_stack s2 s4 in
        return (r1 && r2)
    | _ ->
        let+ () =
          throw `Error
            Text.
              [
                Text "type mismatch between ";
                Any (coalesce lhs, Type_pp.pp_ty);
                Text " and ";
                Any (coalesce rhs, Type_pp.pp_ty);
              ]
        in
        false

  and constrain_stack lhs rhs : bool Diagnosed.t =
    let open Diagnosed in
    let open Simple_type in
    match (lhs, rhs) with
    | SVar a, _ ->
        if not (List.memq rhs a.upper) then (
          a.upper <- rhs :: a.upper;
          List.fold_left
            (fun acc lb ->
              let* acc = acc in
              let* res = constrain_stack lb rhs in
              return (acc && res))
            (return true) a.lower)
        else return true
    | _, SVar b ->
        if not (List.memq lhs b.lower) then (
          b.lower <- lhs :: b.lower;
          List.fold_left
            (fun acc ub ->
              let* acc = acc in
              let* res = constrain_stack lhs ub in
              return (acc && res))
            (return true) b.upper)
        else return true
    | SCons (t1, s1), SCons (t2, s2) ->
        let* rt = constrain_ty t1 t2 in
        let* rs = constrain_stack s1 s2 in
        return (rt && rs)
    | _ ->
        let+ () = throw `Error Text.[ Text "stack mismatch" ] in
        false

  let freshen level =
    let open Simple_type in
    let ty_cache = Hashtbl.create 4 in
    let stk_cache = Hashtbl.create 4 in
    let rec go_ty = function
      | TError -> TError
      | TPrim _ as t -> t
      | TFunc (lhs, rhs) -> TFunc (go_stack lhs, go_stack rhs)
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

  let rec infer ctx level (term : Ast.term) : Simple_type.ty Diagnosed.t =
    let open Diagnosed in
    let open Simple_type in
    let open Ast in
    match term.value with
    | Id ->
        let rho = fresh_stack_var ~level () in
        return (TFunc (rho, rho))
    | Lit l ->
        let rho = fresh_stack_var ~level () in
        return (TFunc (rho, SCons (TPrim (primitive_of_literal l), rho)))
    | Word w -> (
        match String_map.find_opt w ctx.env with
        | Some ty ->
            let rho = fresh_stack_var ~level () in
            return (TFunc (rho, SCons (ty, rho)))
        | None -> (
            match String_map.find_opt w ctx.placeholders with
            | Some ph -> return ph
            | None ->
                let* t =
                  adorn ~span:(Some term.span) (ctx.ask (Query.WordType w))
                in
                return (freshen level t)))
    | Cat (f, g) -> (
        let* tf = infer ctx level f in
        let* tg = infer ctx level g in
        match (tf, tg) with
        | TFunc (s_in, s_mid), TFunc (s_mid', s_out) ->
            let* res =
              adorn ~span:(Some term.span) (constrain_stack s_mid s_mid')
            in
            return @@ if res then TFunc (s_in, s_out) else TFunc (SError, SError)
        | _ -> assert false)
    | Quote f ->
        let* tf = infer ctx level f in
        let rho = fresh_stack_var ~level () in
        return (TFunc (rho, SCons (tf, rho)))
    | Bind (name, body) -> (
        let t = fresh_ty_var ~level () in
        let rho = fresh_stack_var ~level () in
        let* body_ty =
          infer
            { ctx with env = String_map.add name.value t ctx.env }
            level body
        in
        match body_ty with
        | TFunc (body_in, body_out) ->
            let+ _ =
              adorn ~span:(Some term.span) (constrain_stack rho body_in)
            in
            TFunc (SCons (t, rho), body_out)
        | _ -> assert false)
end
