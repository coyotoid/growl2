open Containers
module Int_set = Set.Make (Int)

type polarity = Pos | Neg [@@deriving eq]
type variable = { mutable pos : bool; mutable neg : bool }

let flip = function Pos -> Neg | Neg -> Pos

module Make () = struct
  let uf_store = Store.create ()

  let uf_elems : (int, string UnionFind.Stored.rref) Hashtbl.t =
    Hashtbl.create 8

  let get_uf_elem id =
    match Hashtbl.find_opt uf_elems id with
    | Some e -> e
    | None ->
        let e = UnionFind.Stored.make uf_store "" in
        Hashtbl.add uf_elems id e;
        e

  let merge_uf id1 id2 =
    ignore (UnionFind.Stored.union uf_store (get_uf_elem id1) (get_uf_elem id2))

  let same_class id1 id2 =
    match (Hashtbl.find_opt uf_elems id1, Hashtbl.find_opt uf_elems id2) with
    | Some e1, Some e2 -> UnionFind.Stored.eq uf_store e1 e2
    | _ -> false

  let next_name =
    let n = ref 0 in
    fun () ->
      let i = Ref.get_then_incr n in
      let letter = String.make 1 (Char.chr (Char.code 'a' + (i mod 26))) in
      if i < 26 then letter else letter ^ string_of_int (i / 26)

  let name_of (v : 'a Simple_type.var) =
    let rep = UnionFind.Stored.find uf_store (get_uf_elem v.id) in
    let n = UnionFind.Stored.get uf_store rep in
    if String.is_empty n then (
      let fresh = next_name () in
      UnionFind.Stored.set uf_store rep fresh;
      fresh)
    else n

  let collect_polarities t start_pol =
    let tbl = Hashtbl.create 8 in
    let get id =
      match Hashtbl.find_opt tbl id with
      | Some p -> p
      | None ->
          let p = { pos = false; neg = false } in
          Hashtbl.add tbl id p;
          p
    in
    let ty_seen = Hashtbl.create 8 and st_seen = Hashtbl.create 8 in
    let rec go_ty pol = function
      | Simple_type.TError | Simple_type.TPrim _ -> ()
      | Simple_type.TFunc (lhs, rhs) ->
          go_stack (flip pol) lhs;
          go_stack pol rhs
      | Simple_type.TVar v ->
          let p = get v.id in
          (match pol with Pos -> p.pos <- true | Neg -> p.neg <- true);
          let key = (v.id, equal_polarity pol Pos) in
          if not (Hashtbl.mem ty_seen key) then (
            Hashtbl.add ty_seen key ();
            List.iter (go_ty pol)
              (if equal_polarity pol Pos then v.lower else v.upper))
    and go_stack pol = function
      | Simple_type.SError -> ()
      | Simple_type.SCons (t, s) ->
          go_ty pol t;
          go_stack pol s
      | Simple_type.SVar v ->
          let p = get v.id in
          (match pol with Pos -> p.pos <- true | Neg -> p.neg <- true);
          let key = (v.id, equal_polarity pol Pos) in
          if not (Hashtbl.mem st_seen key) then (
            Hashtbl.add st_seen key ();
            List.iter (go_stack pol)
              (if equal_polarity pol Pos then v.lower else v.upper))
    in
    go_ty start_pol t;
    tbl

  let build_uf biv t =
    let is_both id =
      match Hashtbl.find_opt biv id with
      | Some p -> p.pos && p.neg
      | None -> false
    in
    let ty_done = Hashtbl.create 8 and st_done = Hashtbl.create 8 in
    let rec go_ty = function
      | Simple_type.TError | Simple_type.TPrim _ -> ()
      | Simple_type.TFunc (lhs, rhs) ->
          go_stack lhs;
          go_stack rhs
      | Simple_type.TVar v when is_both v.id ->
          if not (Hashtbl.mem ty_done v.id) then (
            Hashtbl.add ty_done v.id ();
            List.iter
              (function
                | Simple_type.TVar w when is_both w.id -> merge_uf v.id w.id
                | t -> go_ty t)
              (v.lower @ v.upper))
      | Simple_type.TVar _ -> ()
    and go_stack = function
      | Simple_type.SError -> ()
      | Simple_type.SCons (t, s) ->
          go_ty t;
          go_stack s
      | Simple_type.SVar v when is_both v.id ->
          if not (Hashtbl.mem st_done v.id) then (
            Hashtbl.add st_done v.id ();
            List.iter
              (function
                | Simple_type.SVar w when is_both w.id -> merge_uf v.id w.id
                | s -> go_stack s)
              (v.lower @ v.upper))
      | Simple_type.SVar _ -> ()
    in
    go_ty t

  let cv_memo : (string * int, bool) Hashtbl.t = Hashtbl.create 16
  let csv_memo : (string * int, bool) Hashtbl.t = Hashtbl.create 16

  let rec contains_var name : Type.ty -> bool =
   fun t ->
    let key = (name, t.tag) in
    match Hashtbl.find_opt cv_memo key with
    | Some v -> v
    | None ->
        let v =
          match t.node with
          | TError -> false
          | TVar n -> String.equal n name
          | TUnion (lhs, rhs) | TInter (lhs, rhs) ->
              contains_var name lhs || contains_var name rhs
          | TFunc (lhs, rhs) ->
              contains_stack_var name lhs || contains_stack_var name rhs
          | TRec { body; _ } -> contains_var name body
          | TTop | TBot | TPrim _ -> false
        in
        Hashtbl.add cv_memo key v;
        v

  and contains_stack_var name : Type.stack -> bool =
   fun s ->
    let key = (name, s.tag) in
    match Hashtbl.find_opt csv_memo key with
    | Some v -> v
    | None ->
        let v =
          match s.node with
          | SError -> false
          | SVar n -> String.equal n name
          | SUnion (lhs, rhs) | SInter (lhs, rhs) ->
              contains_stack_var name lhs || contains_stack_var name rhs
          | SRec { body; _ } -> contains_stack_var name body
          | SCons (t, s) -> contains_var name t || contains_stack_var name s
          | STop | SBot -> false
        in
        Hashtbl.add csv_memo key v;
        v

  let rec coalesce_ty pol seen vars t =
    match t with
    | Simple_type.TError -> Type.terror
    | Simple_type.TPrim p -> Type.tprim p
    | Simple_type.TFunc (lhs, rhs) ->
        Type.tfunc
          (coalesce_stack (flip pol) seen vars lhs)
          (coalesce_stack pol seen vars rhs)
    | Simple_type.TVar v -> (
        if Int_set.mem v.id seen then Type.tvar (name_of v)
        else
          let seen' = Int_set.add v.id seen in
          let p = Hashtbl.find_opt vars v.id in
          let is_both =
            match p with Some p -> p.pos && p.neg | None -> false
          in
          let n = name_of v in
          let raw_bounds, combine, default =
            match pol with
            | Pos -> (v.lower, (fun a b -> Type.tunion a b), Type.tbot)
            | Neg -> (v.upper, (fun a b -> Type.tinter a b), Type.ttop)
          in
          let bounds =
            if is_both then
              List.filter
                (function
                  | Simple_type.TVar w ->
                      not (Hashtbl.mem vars w.id && same_class v.id w.id)
                  | _ -> true)
                raw_bounds
            else raw_bounds
          in
          let bounds_body =
            match bounds with
            | [] -> None
            | first :: rest ->
                Some
                  (List.fold_left combine
                     (coalesce_ty pol seen' vars first)
                     (List.map (coalesce_ty pol seen' vars) rest))
          in
          match (is_both, bounds_body) with
          | false, None -> default
          | false, Some b -> if contains_var n b then Type.trec n b else b
          | true, None -> Type.tvar n
          | true, Some b ->
              let b' = if contains_var n b then Type.trec n b else b in
              combine (Type.tvar n) b')

  and coalesce_stack pol seen vars s =
    match s with
    | Simple_type.SError -> Type.serror
    | Simple_type.SCons (t, s) ->
        Type.scons (coalesce_ty pol seen vars t) (coalesce_stack pol seen vars s)
    | Simple_type.SVar v -> (
        if Int_set.mem v.id seen then Type.svar (name_of v)
        else
          let seen' = Int_set.add v.id seen in
          let p = Hashtbl.find_opt vars v.id in
          let is_both =
            match p with Some p -> p.pos && p.neg | None -> false
          in
          let n = name_of v in
          let raw_bounds, combine, default =
            match pol with
            | Pos -> (v.lower, (fun a b -> Type.sunion a b), Type.sbot)
            | Neg -> (v.upper, (fun a b -> Type.sinter a b), Type.stop)
          in
          let bounds =
            if is_both then
              List.filter
                (function
                  | Simple_type.SVar w ->
                      not (Hashtbl.mem vars w.id && same_class v.id w.id)
                  | _ -> true)
                raw_bounds
            else raw_bounds
          in
          let bounds_body =
            match bounds with
            | [] -> None
            | first :: rest ->
                Some
                  (List.fold_left combine
                     (coalesce_stack pol seen' vars first)
                     (List.map (coalesce_stack pol seen' vars) rest))
          in
          match (is_both, bounds_body) with
          | false, None -> default
          | false, Some b -> if contains_stack_var n b then Type.srec n b else b
          | true, None -> Type.svar n
          | true, Some b ->
              let b' = if contains_stack_var n b then Type.srec n b else b in
              combine (Type.svar n) b')

  let coalesce t =
    let vars = collect_polarities t Pos in
    build_uf vars t;
    coalesce_ty Pos Int_set.empty vars t
end
