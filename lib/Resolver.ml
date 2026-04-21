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
      Parser_intf.parse_string ~filename:path source
  | Query.SCCs () ->
      let files = ask db (Query.Manifest ()) in
      let graph = Hashtbl.create 16 in
      List.iter
        (fun file_id ->
          match ask db (Query.ParsedProgram file_id) with
          | d when Diagnosed.has `Error d -> ()
          | d ->
              let prog, _ = Diagnosed.run d in
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
  | Query.WordExpr name -> (
      let files = ask db (Query.Manifest ()) in
      let found =
        List.find_map
          (fun file_id ->
            let res = ask db (Query.ParsedProgram file_id) in
            if Diagnosed.has `Error res then None
            else
              let prog = Diagnosed.run res |> Pair.fst in
              List.find_map
                (fun (def : Ast.def Span.Spanned.t) ->
                  if String.equal def.value.name.value name then
                    Some (Diagnosed.return def.value.body)
                  else None)
                prog)
          files
      in
      match found with
      | Some d -> Diagnosed.map Option.some d
      | None -> Diagnosed.return None)
  | Query.WordType name -> (
      match Hashtbl.find_opt db.types_in_progress name with
      | Some (placeholder, is_rec) ->
          is_rec := true;
          Diagnosed.return placeholder
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
          let module I = Inference.Make () in
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
            List.map
              (fun (w, _, _) ->
                let placeholders =
                  List.fold_left
                    (fun m (w2, ph, _) ->
                      if String.equal w2 w then m else String_map.add w2 ph m)
                    String_map.empty entries
                in
                let ctx =
                  Inference.
                    {
                      env = String_map.empty;
                      ask = (fun q -> ask db q);
                      placeholders;
                    }
                in
                let result =
                  let open Diagnosed in
                  let* expr = ask db (Query.WordExpr w) in
                  match expr with
                  | Some expr -> I.infer ctx 0 expr
                  | None ->
                      let+ () =
                        throw `Error Text.[ Text "unbound word: "; Verbatim w ]
                      in
                      Simple_type.(TFunc (SError, SError))
                in
                (w, result))
              entries
          in
          List.iter2
            (fun (_, ph, _) (_, result) ->
              let ty = Diagnosed.run result |> Pair.fst in
              ignore (I.constrain_ty ph ty);
              ignore (I.constrain_ty ty ph))
            entries results;
          List.iter
            (fun (w, _, _) -> Hashtbl.remove db.types_in_progress w)
            entries;
          List.iter
            (fun (w, result) ->
              if not (String.equal w name) then
                Db.store db (Query.WordType w) result [])
            results;
          List.assoc ~eq:String.equal name results)
