module Int_set = Set.Make (Int)
module Int_map = Map.Make (Int)
module Prim_set = Set.Make (Type_primitive)

type ty = {
  vars : Int_set.t;
  prims : Prim_set.t;
  func : (stack * stack) option;
  con : (string * ty list) option;
}

and stack = { svars : Int_set.t; cons : (ty * stack) option }

type scheme = {
  cty : ty;
  rec_ty_vars : ty Int_map.t;
  rec_st_vars : stack Int_map.t;
}

let empty_ty =
  { vars = Int_set.empty; prims = Prim_set.empty; func = None; con = None }

let empty_stack = { svars = Int_set.empty; cons = None }

let rec merge_ty pol a b =
  let func =
    match (a.func, b.func) with
    | None, x | x, None -> x
    | Some (al, ar), Some (bl, br) ->
        Some (merge_stack (not pol) al bl, merge_stack pol ar br)
  in
  let con =
    match (a.con, b.con) with
    | None, x | x, None -> x
    | Some (c1, args1), Some (c2, args2) when String.equal c1 c2 ->
        Some (c1, List.map2 (merge_ty pol) args1 args2)
    | Some _, Some _ -> None
  in
  {
    vars = Int_set.union a.vars b.vars;
    prims = Prim_set.union a.prims b.prims;
    func;
    con;
  }

and merge_stack pol a b =
  let cons =
    match (a.cons, b.cons) with
    | None, x | x, None -> x
    | Some (at_, as_), Some (bt, bs) ->
        Some (merge_ty pol at_ bt, merge_stack pol as_ bs)
  in
  { svars = Int_set.union a.svars b.svars; cons }

module Polar = struct
  type t = int * bool [@@deriving ord]
end

module Polar_set = Set.Make (Polar)

let compact (root : Simple_type.ty) : scheme =
  let rec_ty : (int * bool, unit) Hashtbl.t = Hashtbl.create 4 in
  let rec_st : (int * bool, unit) Hashtbl.t = Hashtbl.create 4 in
  let rec_ty_bounds : (int, ty) Hashtbl.t = Hashtbl.create 4 in
  let rec_st_bounds : (int, stack) Hashtbl.t = Hashtbl.create 4 in

  let rec go_ty ty pol parents in_process =
    match ty with
    | Simple_type.TError -> empty_ty
    | Simple_type.TPrim p -> { empty_ty with prims = Prim_set.singleton p }
    | Simple_type.TFunc (lhs, rhs) ->
        let l = go_stack lhs (not pol) Int_set.empty in_process in
        let r = go_stack rhs pol Int_set.empty in_process in
        { empty_ty with func = Some (l, r) }
    | Simple_type.TCon (name, args) ->
        let args' =
          List.map (fun a -> go_ty a pol Int_set.empty in_process) args
        in
        { empty_ty with con = Some (name, args') }
    | Simple_type.TVar v ->
        let key = (v.id, pol) in
        if Polar_set.mem key in_process then
          if Int_set.mem v.id parents then empty_ty
          else begin
            Hashtbl.replace rec_ty key ();
            { empty_ty with vars = Int_set.singleton v.id }
          end
        else if Polar_set.mem (v.id, not pol) in_process then begin
          Hashtbl.replace rec_ty (v.id, pol) ();
          Hashtbl.replace rec_ty (v.id, not pol) ();
          { empty_ty with vars = Int_set.singleton v.id }
        end
        else
          let in_process' = Polar_set.add key in_process in
          let bounds = if pol then v.lower else v.upper in
          let base = { empty_ty with vars = Int_set.singleton v.id } in
          let bound =
            List.fold_left
              (fun acc b ->
                merge_ty pol acc
                  (go_ty b pol (Int_set.add v.id parents) in_process'))
              base bounds
          in
          if Hashtbl.mem rec_ty key then begin
            Hashtbl.replace rec_ty_bounds v.id bound;
            { empty_ty with vars = Int_set.singleton v.id }
          end
          else bound
  and go_stack st pol parents in_process =
    match st with
    | Simple_type.SError -> empty_stack
    | Simple_type.SCons (t, s) ->
        let t' = go_ty t pol Int_set.empty in_process in
        let s' = go_stack s pol Int_set.empty in_process in
        { empty_stack with cons = Some (t', s') }
    | Simple_type.SVar v ->
        let key = (v.id, pol) in
        if Polar_set.mem key in_process then
          if Int_set.mem v.id parents then empty_stack
          else begin
            Hashtbl.replace rec_st key ();
            { empty_stack with svars = Int_set.singleton v.id }
          end
        else if Polar_set.mem (v.id, not pol) in_process then begin
          Hashtbl.replace rec_st (v.id, pol) ();
          Hashtbl.replace rec_st (v.id, not pol) ();
          { empty_stack with svars = Int_set.singleton v.id }
        end
        else
          let in_process' = Polar_set.add key in_process in
          let bounds = if pol then v.lower else v.upper in
          let base = { empty_stack with svars = Int_set.singleton v.id } in
          let bound =
            List.fold_left
              (fun acc b ->
                merge_stack pol acc
                  (go_stack b pol (Int_set.add v.id parents) in_process'))
              base bounds
          in
          if Hashtbl.mem rec_st key then begin
            Hashtbl.replace rec_st_bounds v.id bound;
            { empty_stack with svars = Int_set.singleton v.id }
          end
          else bound
  in

  let cty = go_ty root true Int_set.empty Polar_set.empty in
  let rec_ty_vars =
    Hashtbl.fold
      (fun id b acc -> Int_map.add id b acc)
      rec_ty_bounds Int_map.empty
  in
  let rec_st_vars =
    Hashtbl.fold
      (fun id b acc -> Int_map.add id b acc)
      rec_st_bounds Int_map.empty
  in
  { cty; rec_ty_vars; rec_st_vars }

let simplify (scm : scheme) : scheme =
  let co_occ_ty : (int * bool, Int_set.t * Prim_set.t) Hashtbl.t =
    Hashtbl.create 16
  in
  let co_occ_st : (int * bool, Int_set.t) Hashtbl.t = Hashtbl.create 16 in
  let all_ty_vars : (int, unit) Hashtbl.t = Hashtbl.create 16 in
  let all_st_vars : (int, unit) Hashtbl.t = Hashtbl.create 16 in
  let rec_ty_seen : (int, unit) Hashtbl.t = Hashtbl.create 4 in
  let rec_st_seen : (int, unit) Hashtbl.t = Hashtbl.create 4 in

  let update_co_occ_ty id pol new_vars new_prims =
    let key = (id, pol) in
    match Hashtbl.find_opt co_occ_ty key with
    | None -> Hashtbl.add co_occ_ty key (new_vars, new_prims)
    | Some (vs, ps) ->
        Hashtbl.replace co_occ_ty key
          (Int_set.inter vs new_vars, Prim_set.inter ps new_prims)
  in

  let update_co_occ_st id pol new_svars =
    let key = (id, pol) in
    match Hashtbl.find_opt co_occ_st key with
    | None -> Hashtbl.add co_occ_st key new_svars
    | Some vs -> Hashtbl.replace co_occ_st key (Int_set.inter vs new_svars)
  in

  let rec go_ty (t : ty) pol =
    Int_set.iter
      (fun v ->
        Hashtbl.replace all_ty_vars v ();
        update_co_occ_ty v pol t.vars t.prims;
        Option.iter
          (fun bound ->
            if not (Hashtbl.mem rec_ty_seen v) then begin
              Hashtbl.add rec_ty_seen v ();
              go_ty bound pol
            end)
          (Int_map.find_opt v scm.rec_ty_vars))
      t.vars;
    Option.iter
      (fun (l, r) ->
        go_stack l (not pol);
        go_stack r pol)
      t.func;
    Option.iter
      (fun (_name, args) -> List.iter (fun a -> go_ty a pol) args)
      t.con
  and go_stack (s : stack) pol =
    Int_set.iter
      (fun v ->
        Hashtbl.replace all_st_vars v ();
        update_co_occ_st v pol s.svars;
        Option.iter
          (fun bound ->
            if not (Hashtbl.mem rec_st_seen v) then begin
              Hashtbl.add rec_st_seen v ();
              go_stack bound pol
            end)
          (Int_map.find_opt v scm.rec_st_vars))
      s.svars;
    Option.iter
      (fun (t, s') ->
        go_ty t pol;
        go_stack s' pol)
      s.cons
  in

  go_ty scm.cty true;

  let ty_subst : (int, int option) Hashtbl.t = Hashtbl.create 16 in
  let st_subst : (int, int option) Hashtbl.t = Hashtbl.create 16 in

  let new_rec_ty : (int, ty) Hashtbl.t = Hashtbl.create 4 in
  Int_map.iter (Hashtbl.add new_rec_ty) scm.rec_ty_vars;
  let new_rec_st : (int, stack) Hashtbl.t = Hashtbl.create 4 in
  Int_map.iter (Hashtbl.add new_rec_st) scm.rec_st_vars;

  Hashtbl.iter
    (fun v () ->
      if (not (Hashtbl.mem ty_subst v)) && not (Hashtbl.mem new_rec_ty v) then
        match
          ( Hashtbl.find_opt co_occ_ty (v, true),
            Hashtbl.find_opt co_occ_ty (v, false) )
        with
        | Some _, None | None, Some _ -> Hashtbl.replace ty_subst v None
        | _ -> ())
    all_ty_vars;

  Hashtbl.iter
    (fun v () ->
      if (not (Hashtbl.mem st_subst v)) && not (Hashtbl.mem new_rec_st v) then
        match
          ( Hashtbl.find_opt co_occ_st (v, true),
            Hashtbl.find_opt co_occ_st (v, false) )
        with
        | Some _, None | None, Some _ -> Hashtbl.replace st_subst v None
        | _ -> ())
    all_st_vars;

  let is_eliminated_ty v = Hashtbl.mem ty_subst v in
  let is_eliminated_st v = Hashtbl.mem st_subst v in

  let has_conflicting_prims v pol =
    match
      ( Hashtbl.find_opt co_occ_ty (v, pol),
        Hashtbl.find_opt co_occ_ty (v, not pol) )
    with
    | Some (_, ps_here), Some (_, ps_opp) ->
        not (Prim_set.is_empty (Prim_set.inter ps_here ps_opp))
    | _ -> false
  in

  let try_merge_ty_var pol v w =
    if w = v then ()
    else if not (Hashtbl.mem ty_subst w) then
      match Hashtbl.find_opt co_occ_ty (w, pol) with
      | None -> ()
      | Some (w_co_vars, _) ->
          if not (Int_set.mem v w_co_vars) then ()
          else begin
            Hashtbl.replace ty_subst w (Some v);
            if Hashtbl.mem new_rec_ty w then begin
              let b_w = Hashtbl.find new_rec_ty w in
              let b_v = Option.value ~default:empty_ty (Hashtbl.find_opt new_rec_ty v) in
              Hashtbl.replace new_rec_ty v (merge_ty pol b_v b_w);
              Hashtbl.remove new_rec_ty w
            end
            else
              begin match Hashtbl.find_opt co_occ_ty (v, not pol) with
              | None -> ()
              | Some (v_opp_vars, v_opp_prims) -> (
                  match Hashtbl.find_opt co_occ_ty (w, not pol) with
                  | None -> ()
                  | Some (w_opp_vars, w_opp_prims) ->
                      Hashtbl.replace co_occ_ty (v, not pol)
                        ( Int_set.add v (Int_set.inter v_opp_vars w_opp_vars),
                          Prim_set.inter v_opp_prims w_opp_prims ))
              end
          end
  in

  let try_merge_st_var pol v w =
    if w = v then ()
    else if not (Hashtbl.mem st_subst w) then
      match Hashtbl.find_opt co_occ_st (w, pol) with
      | None -> ()
      | Some w_co_svars ->
          if not (Int_set.mem v w_co_svars) then ()
          else begin
            Hashtbl.replace st_subst w (Some v);
            if Hashtbl.mem new_rec_st w then begin
              let b_w = Hashtbl.find new_rec_st w in
              let b_v = Option.value ~default:empty_stack (Hashtbl.find_opt new_rec_st v) in
              Hashtbl.replace new_rec_st v (merge_stack pol b_v b_w);
              Hashtbl.remove new_rec_st w
            end
            else
              begin match Hashtbl.find_opt co_occ_st (v, not pol) with
              | None -> ()
              | Some v_opp -> (
                  match Hashtbl.find_opt co_occ_st (w, not pol) with
                  | None -> ()
                  | Some w_opp ->
                      Hashtbl.replace co_occ_st (v, not pol)
                        (Int_set.add v (Int_set.inter v_opp w_opp)))
              end
          end
  in

  let merge_co_occurring_ty_vars v pol =
    match Hashtbl.find_opt co_occ_ty (v, pol) with
    | None -> ()
    | Some (co_vars, _) -> Int_set.iter (try_merge_ty_var pol v) co_vars
  in

  let merge_co_occurring_st_vars v pol =
    match Hashtbl.find_opt co_occ_st (v, pol) with
    | None -> ()
    | Some co_svars -> Int_set.iter (try_merge_st_var pol v) co_svars
  in

  let eliminate_ty_var v =
    if is_eliminated_ty v then ()
    else if not (Hashtbl.mem new_rec_ty v) then
      if has_conflicting_prims v true || has_conflicting_prims v false then
        Hashtbl.replace ty_subst v None
      else begin
        merge_co_occurring_ty_vars v true;
        if not (is_eliminated_ty v) then merge_co_occurring_ty_vars v false
      end
    else begin
      merge_co_occurring_ty_vars v true;
      if not (is_eliminated_ty v) then merge_co_occurring_ty_vars v false
    end
  in

  let eliminate_st_var v =
    if is_eliminated_st v then ()
    else begin
      merge_co_occurring_st_vars v true;
      if not (is_eliminated_st v) then merge_co_occurring_st_vars v false
    end
  in

  Hashtbl.iter (fun v () -> eliminate_ty_var v) all_ty_vars;
  Hashtbl.iter (fun v () -> eliminate_st_var v) all_st_vars;

  let resolve_ty v =
    match Hashtbl.find_opt ty_subst v with
    | None -> Some v
    | Some None -> None
    | Some (Some w) -> Some w
  in

  let resolve_st v =
    match Hashtbl.find_opt st_subst v with
    | None -> Some v
    | Some None -> None
    | Some (Some w) -> Some w
  in

  let filter_map_int_set resolve s =
    Int_set.fold
      (fun v acc ->
        match resolve v with Some w -> Int_set.add w acc | None -> acc)
      s Int_set.empty
  in

  let rec rebuild_ty (t : ty) : ty =
    let vars = filter_map_int_set resolve_ty t.vars in
    let func =
      Option.map (fun (l, r) -> (rebuild_stack l, rebuild_stack r)) t.func
    in
    let con =
      Option.map (fun (name, args) -> (name, List.map rebuild_ty args)) t.con
    in
    { t with vars; func; con }
  and rebuild_stack (s : stack) : stack =
    let svars = filter_map_int_set resolve_st s.svars in
    let cons =
      Option.map (fun (t, s') -> (rebuild_ty t, rebuild_stack s')) s.cons
    in
    { svars; cons }
  in

  let cty = rebuild_ty scm.cty in
  let rec_ty_vars =
    Hashtbl.fold
      (fun v bound acc ->
        match resolve_ty v with
        | Some v' when v' = v -> Int_map.add v (rebuild_ty bound) acc
        | _ -> acc)
      new_rec_ty Int_map.empty
  in
  let rec_st_vars =
    Hashtbl.fold
      (fun v bound acc ->
        match resolve_st v with
        | Some v' when v' = v -> Int_map.add v (rebuild_stack bound) acc
        | _ -> acc)
      new_rec_st Int_map.empty
  in
  { cty; rec_ty_vars; rec_st_vars }
