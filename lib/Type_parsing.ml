let simple_ty_of_annot (module I : Type_inference.S) (ann : Type_ast.annot) =
  let ty_vars = Hashtbl.create 4 in
  let stk_vars = Hashtbl.create 4 in
  let fresh_ty name =
    match Hashtbl.find_opt ty_vars name with
    | Some v -> Simple_type.TVar v
    | None ->
        let v =
          Simple_type.{ id = I.next_id (); upper = []; lower = []; level = 0 }
        in
        Hashtbl.add ty_vars name v;
        Simple_type.TVar v
  in
  let fresh_stack name =
    match Hashtbl.find_opt stk_vars name with
    | Some v -> Simple_type.SVar v
    | None ->
        let v =
          Simple_type.{ id = I.next_id (); upper = []; lower = []; level = 0 }
        in
        Hashtbl.add stk_vars name v;
        Simple_type.SVar v
  in

  let rec go_ty = function
    | Type_ast.TPrim p -> Simple_type.TPrim p
    | Type_ast.TVar n -> fresh_ty n
    | Type_ast.TCon (n, args) -> Simple_type.TCon (n, List.map go_ty args)
    | Type_ast.TFunc { inputs; outputs } ->
        Simple_type.TFunc (go_stack inputs, go_stack outputs)
  and go_stack items =
    let base, items =
      match items with
      | Type_ast.SRest n :: rest -> (fresh_stack n, rest)
      | _ -> (I.fresh_stack_var ~level:0 (), items)
    in
    List.fold_left
      (fun acc item ->
        match item with
        | Type_ast.SItem t -> Simple_type.SCons (go_ty t, acc)
        | Type_ast.SRest _ ->
            failwith "stack variable must appear at the bottom")
      base items
  in

  Simple_type.TFunc (go_stack ann.inputs, go_stack ann.outputs)

let annot_to_type (ann : Type_ast.annot) : Type.ty =
  let anon_counter = ref 0 in
  let fresh_anon () =
    let i = !anon_counter in
    incr anon_counter;
    Type.svar (Printf.sprintf "_r%d" i)
  in
  let rec go_ty = function
    | Type_ast.TPrim p -> Type.tprim p
    | Type_ast.TVar n -> Type.tvar n
    | Type_ast.TCon (n, args) -> Type.tcon n (List.map go_ty args)
    | Type_ast.TFunc { inputs; outputs } ->
        Type.tfunc (go_stack inputs) (go_stack outputs)
  and go_stack items =
    let base, rest =
      match items with
      | Type_ast.SRest n :: tl -> (Type.svar n, tl)
      | _ -> (fresh_anon (), items)
    in
    List.fold_left
      (fun acc -> function
        | Type_ast.SItem t -> Type.scons (go_ty t) acc
        | Type_ast.SRest _ ->
            failwith "stack variable must appear at the bottom")
      base rest
  in
  Type.tfunc (go_stack ann.inputs) (go_stack ann.outputs)

let check_ann_shape (ann : Type.ty) (inferred : Type.ty) : bool =
  let ty_binds : (string, Type.ty) Hashtbl.t = Hashtbl.create 4 in
  let stk_binds : (string, Type.stack) Hashtbl.t = Hashtbl.create 4 in
  let loose_ty (prev : Type.ty) (cur : Type.ty) =
    match (prev.node, cur.node) with
    | TVar _, TVar _ -> true
    | _ -> prev.tag = cur.tag
  in
  let loose_stk (prev : Type.stack) (cur : Type.stack) =
    match (prev.node, cur.node) with
    | SVar _, SVar _ -> true
    | _ -> prev.tag = cur.tag
  in
  let rec go_ty (a : Type.ty) (i : Type.ty) =
    if i.node = Type.TTop then true
    else
      match a.node with
      | TVar name -> (
          match Hashtbl.find_opt ty_binds name with
          | None ->
              Hashtbl.add ty_binds name i;
              true
          | Some p -> loose_ty p i)
      | TTop -> true
      | _ when i.node = TBot -> true
      | TPrim p -> (
          match i.node with TPrim q -> Type_primitive.equal p q | _ -> false)
      | TFunc (sa, sb) -> (
          match i.node with
          | Type.TFunc (si, so) -> go_stk sa si && go_stk sb so
          | _ -> false)
      | TCon (n1, a1) -> (
          match i.node with
          | TCon (n2, a2) when String.equal n1 n2 ->
              List.length a1 = List.length a2 && List.for_all2 go_ty a1 a2
          | _ -> false)
      | _ -> false
  and go_stk (a : Type.stack) (i : Type.stack) =
    if i.node = STop then true
    else
      match a.node with
      | SVar name -> (
          match Hashtbl.find_opt stk_binds name with
          | None ->
              Hashtbl.add stk_binds name i;
              true
          | Some p -> loose_stk p i)
      | SBot -> true
      | STop -> true
      | SCons (at, as_) -> (
          match i.node with
          | SCons (it, is_) -> go_ty at it && go_stk as_ is_
          | _ -> false)
      | _ -> false
  in
  go_ty ann inferred
