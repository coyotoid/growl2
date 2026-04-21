open Containers
module Int_set = Set.Make (Int)

type polarity = Pos | Neg [@@deriving eq]
type variable = { mutable pos : bool; mutable neg : bool }

let flip = function Pos -> Neg | Neg -> Pos

module Make () = struct
  let ty_names : (int, string) Hashtbl.t = Hashtbl.create 8
  let st_names : (int, string) Hashtbl.t = Hashtbl.create 8

  let next_ty_name =
    let n = ref 0 in
    fun () ->
      let i = Ref.get_then_incr n in
      let letter = String.make 1 (Char.chr (Char.code 'a' + (i mod 26))) in
      if i < 26 then letter else letter ^ string_of_int (i / 26)

  let next_st_name =
    let n = ref 0 in
    fun () ->
      let i = Ref.get_then_incr n in
      let letter = String.make 1 (Char.chr (Char.code 'r' + (i mod 9))) in
      if i < 9 then letter else letter ^ string_of_int (i / 9)

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

  let name_of_ty_id id =
    match Hashtbl.find_opt ty_names id with
    | Some n -> n
    | None ->
        let fresh = next_ty_name () in
        Hashtbl.add ty_names id fresh;
        fresh

  let name_of_st_id id =
    match Hashtbl.find_opt st_names id with
    | Some n -> n
    | None ->
        let fresh = next_st_name () in
        Hashtbl.add st_names id fresh;
        fresh

  let assign_names (sch : Compact_type.scheme) =
    let ty_seen : (int, unit) Hashtbl.t = Hashtbl.create 8 in
    let st_seen : (int, unit) Hashtbl.t = Hashtbl.create 8 in
    let rec scan_ty (t : Compact_type.ty) =
      Compact_type.Int_set.iter
        (fun v ->
          ignore (name_of_ty_id v);
          if not (Hashtbl.mem ty_seen v) then begin
            Hashtbl.add ty_seen v ();
            Option.iter scan_ty
              (Compact_type.Int_map.find_opt v sch.rec_ty_vars)
          end)
        t.vars;
      Option.iter
        (fun (l, r) ->
          scan_stack l;
          scan_stack r)
        t.func
    and scan_stack (s : Compact_type.stack) =
      Option.iter
        (fun (t, s') ->
          scan_stack s';
          scan_ty t)
        s.cons;
      Compact_type.Int_set.iter
        (fun v ->
          ignore (name_of_st_id v);
          if not (Hashtbl.mem st_seen v) then begin
            Hashtbl.add st_seen v ();
            Option.iter scan_stack
              (Compact_type.Int_map.find_opt v sch.rec_st_vars)
          end)
        s.svars
    in
    scan_ty sch.cty

  let collect_polarities (sch : Compact_type.scheme) =
    let ty_pols : (int, variable) Hashtbl.t = Hashtbl.create 16 in
    let st_pols : (int, variable) Hashtbl.t = Hashtbl.create 16 in
    let seen_ty : (int * bool, unit) Hashtbl.t = Hashtbl.create 8 in
    let seen_st : (int * bool, unit) Hashtbl.t = Hashtbl.create 8 in
    let get tbl id =
      match Hashtbl.find_opt tbl id with
      | Some p -> p
      | None ->
          let p = { pos = false; neg = false } in
          Hashtbl.add tbl id p;
          p
    in
    let rec scan_ty (t : Compact_type.ty) pol =
      Compact_type.Int_set.iter
        (fun v ->
          let p = get ty_pols v in
          (match pol with Pos -> p.pos <- true | Neg -> p.neg <- true);
          let key = (v, equal_polarity pol Pos) in
          if not (Hashtbl.mem seen_ty key) then begin
            Hashtbl.add seen_ty key ();
            Option.iter
              (fun b -> scan_ty b pol)
              (Compact_type.Int_map.find_opt v sch.rec_ty_vars)
          end)
        t.vars;
      Option.iter
        (fun (l, r) ->
          scan_stack l (flip pol);
          scan_stack r pol)
        t.func
    and scan_stack (s : Compact_type.stack) pol =
      Compact_type.Int_set.iter
        (fun v ->
          let p = get st_pols v in
          (match pol with Pos -> p.pos <- true | Neg -> p.neg <- true);
          let key = (v, equal_polarity pol Pos) in
          if not (Hashtbl.mem seen_st key) then begin
            Hashtbl.add seen_st key ();
            Option.iter
              (fun b -> scan_stack b pol)
              (Compact_type.Int_map.find_opt v sch.rec_st_vars)
          end)
        s.svars;
      Option.iter
        (fun (t, s') ->
          scan_ty t pol;
          scan_stack s' pol)
        s.cons
    in
    scan_ty sch.cty Pos;
    (ty_pols, st_pols)

  let coalesce (sch : Compact_type.scheme) : Type.ty =
    assign_names sch;
    let ty_pols, st_pols = collect_polarities sch in
    let is_ty_both v =
      match Hashtbl.find_opt ty_pols v with
      | Some p -> p.pos && p.neg
      | None -> false
    in
    let is_st_both v =
      match Hashtbl.find_opt st_pols v with
      | Some p -> p.pos && p.neg
      | None -> false
    in
    let rec go_ty (t : Compact_type.ty) pol seen_ty seen_st =
      let combine, default =
        match pol with
        | Pos -> ((fun a b -> Type.tunion a b), Type.tbot)
        | Neg -> ((fun a b -> Type.tinter a b), Type.ttop)
      in
      let parts =
        Compact_type.Prim_set.fold (fun p acc -> Type.tprim p :: acc) t.prims []
      in
      let parts =
        Compact_type.Int_set.fold
          (fun v acc ->
            let n = name_of_ty_id v in
            let ty_v =
              if Int_set.mem v seen_ty then Type.tvar n
              else
                match Compact_type.Int_map.find_opt v sch.rec_ty_vars with
                | None -> Type.tvar n
                | Some bound ->
                    let seen_ty' = Int_set.add v seen_ty in
                    let b = go_ty bound pol seen_ty' seen_st in
                    let b' = if contains_var n b then Type.trec n b else b in
                    if is_ty_both v then combine (Type.tvar n) b' else b'
            in
            ty_v :: acc)
          t.vars parts
      in
      let parts =
        match t.func with
        | None -> parts
        | Some (l, r) ->
            Type.tfunc
              (go_stack l (flip pol) seen_ty seen_st)
              (go_stack r pol seen_ty seen_st)
            :: parts
      in
      List.fold_left combine default parts
    and go_stack (s : Compact_type.stack) pol seen_ty seen_st =
      let combine, default =
        match pol with
        | Pos -> ((fun a b -> Type.sunion a b), Type.sbot)
        | Neg -> ((fun a b -> Type.sinter a b), Type.stop)
      in
      let parts =
        Compact_type.Int_set.fold
          (fun v acc ->
            let n = name_of_st_id v in
            let st_v =
              if Int_set.mem v seen_st then Type.svar n
              else
                match Compact_type.Int_map.find_opt v sch.rec_st_vars with
                | None -> Type.svar n
                | Some bound ->
                    let seen_st' = Int_set.add v seen_st in
                    let b = go_stack bound pol seen_ty seen_st' in
                    let b' =
                      if contains_stack_var n b then Type.srec n b else b
                    in
                    if is_st_both v then combine (Type.svar n) b' else b'
            in
            st_v :: acc)
          s.svars []
      in
      let parts =
        match s.cons with
        | None -> parts
        | Some (t, s') ->
            Type.scons
              (go_ty t pol seen_ty seen_st)
              (go_stack s' pol seen_ty seen_st)
            :: parts
      in
      List.fold_left combine default parts
    in
    go_ty sch.cty Pos Int_set.empty Int_set.empty
end
