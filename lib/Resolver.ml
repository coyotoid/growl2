open Containers
module String_map = Map.Make (String)
module String_table = Hashtbl.Make (String)

let word_refs (term : Ast.term) : string Containers_scc.iter =
 fun yield ->
  let rec go t =
    match t.Span.Spanned.value with
    | Ast.Word w -> yield w
    | Ast.Cat (f, g) ->
        go f;
        go g
    | Ast.Quote f -> go f
    | Ast.Bind (_, f) -> go f
    | _ -> ()
  in
  go term

let coalesce_ty t =
  let module C = Type_coalescing.Make () in
  t |> Compact_type.compact |> Compact_type.simplify |> C.coalesce
  |> Type.simplify_ty

let rec ask : type a. Db.t -> a Query.t -> a =
 fun db q ->
  match Db.find db q with
  | Some v ->
      db.current_deps <- Db.Key.Pack q :: db.current_deps;
      v
  | None ->
      let parent_deps = db.current_deps in
      db.current_deps <- [];
      let v = compute db q in
      let my_deps = db.current_deps in
      db.current_deps <- Db.Key.Pack q :: parent_deps;
      Db.store db q v my_deps;
      v

and compute : type a. Db.t -> a Query.t -> a =
 fun db -> function
  | Query.Manifest () -> failwith "base input"
  | Query.SourceText _ -> failwith "base input"
  | Query.ParsedProgram (Query.FileId path) ->
      let source = ask db (Query.SourceText (Query.FileId path)) in
      Diagnosed.run (fun () -> Parser_intf.parse_string ~filename:path source)
      |> Diagnosed.raise
  | Query.SCCs () ->
      let files = ask db (Query.Manifest ()) in
      let graph = Hashtbl.create 16 in
      List.iter
        (fun file_id ->
          let prog = ask db (Query.ParsedProgram file_id) in
          List.iter
            (fun (def : Ast.def Span.Spanned.t) ->
              Hashtbl.replace graph def.value.name.value def.value.body)
            prog)
        files;
      let nodes = Hashtbl.fold (fun k _ acc -> k :: acc) graph [] in
      Containers_scc.scc
        ~tbl:(module String_table)
        ~graph
        ~children:(fun defs name ->
          match Hashtbl.find_opt defs name with
          | None -> fun _ -> ()
          | Some term -> word_refs term)
        ~nodes ()
  | Query.WordDef name ->
      let files = ask db (Query.Manifest ()) in
      List.find_map
        (fun file_id ->
          let prog = ask db (Query.ParsedProgram file_id) in
          List.find_map
            (fun (def : Ast.def Span.Spanned.t) ->
              if String.equal def.value.name.value name then Some def else None)
            prog)
        files
  | Query.WordType name -> (
      match Hashtbl.find_opt db.types_in_progress name with
      | Some (placeholder, is_rec) ->
          is_rec := true;
          placeholder
      | None ->
          let scc =
            match
              List.find_opt
                (List.exists (String.equal name))
                (ask db (Query.SCCs ()))
            with
            | Some s -> s
            | None -> [ name ]
          in
          let module I = Type_inference.Make () in
          let entries =
            List.map
              (fun w ->
                let ph =
                  Simple_type.TFunc
                    ( I.fresh_stack_var ~level:0 (),
                      I.fresh_stack_var ~level:0 () )
                in
                let is_rec = ref false in
                Hashtbl.add db.types_in_progress w (ph, is_rec);
                (w, ph, is_rec))
              scc
          in
          let results =
            Diagnosed.run (fun () ->
                List.map
                  (fun (w, _, _) ->
                    let placeholders =
                      List.fold_left
                        (fun m (w2, ph, _) ->
                          if String.equal w2 w then m
                          else String_map.add w2 ph m)
                        String_map.empty entries
                    in
                    let ctx =
                      Type_inference.
                        {
                          env = String_map.empty;
                          ask = (fun q -> ask db q);
                          placeholders;
                        }
                    in
                    let def = ask db (Query.WordDef w) in
                    let result =
                      let open Diagnosed in
                      match def with
                      | Some def -> I.infer ctx 0 def.value.body
                      | None ->
                          let () =
                            throw `Error
                              Text.[ Text "unbound word: "; Verbatim w ]
                          in
                          Simple_type.(TFunc (SError, SError))
                    in
                    let result =
                      match def with
                      | Some { value = { annot = Some ann; _ }; span } ->
                          let coalesced = coalesce_ty result in
                          let ann_type  = Type_parsing.annot_to_type ann in
                          if Type_parsing.check_ann_shape ann_type coalesced then
                            Type_parsing.simple_ty_of_annot (module I) ann
                          else
                            let () =
                              Diagnosed.adorn ~span (fun () ->
                                Diagnosed.throw `Error
                                  Text.[
                                    Text "annotation ";
                                    Any (ann_type, Type_pp.pp_ty);
                                    Text " does not match inferred type ";
                                    Any (coalesced, Type_pp.pp_ty);
                                  ])
                            in
                            Simple_type.(TFunc (SError, SError))
                      | _ -> result
                    in
                    (w, result))
                  entries)
            |> Diagnosed.raise
          in
          let final_results =
            List.map2
              (fun (_, ph, _) (w, ty) ->
                let body_span =
                  match ask db (Query.WordDef w) with
                  | Some def -> def.value.body.span
                  | None -> Span.dummy
                in
                let ok =
                  Diagnosed.adorn ~span:body_span (fun () ->
                      let ok1 = I.constrain_ty ph ty in
                      let ok2 = I.constrain_ty ty ph in
                      ok1 && ok2)
                in
                let final_ty =
                  if ok then ty else Simple_type.(TFunc (SError, SError))
                in
                (w, final_ty))
              entries results
          in
          List.iter
            (fun (w, _, _) -> Hashtbl.remove db.types_in_progress w)
            entries;
          List.iter
            (fun (w, result) ->
              if not (String.equal w name) then
                Db.store db (Query.WordType w) result [])
            final_results;
          List.assoc ~eq:String.equal name final_results)
