open Containers
module String_map = Map.Make (String)

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
          let module I = Inference.Make () in
          let placeholder =
            Simple_type.TFunc
              (I.fresh_stack_var ~level:0 (), I.fresh_stack_var ~level:0 ())
          in
          let is_rec = ref false in
          Hashtbl.add db.types_in_progress name (placeholder, is_rec);
          let ctx =
            Inference.{ env = String_map.empty; ask = (fun q -> ask db q) }
          in
          let result =
            let open Diagnosed in
            let* expr = ask db (Query.WordExpr name) in
            match expr with
            | Some expr -> I.infer ctx 0 expr
            | None ->
                let+ () =
                  throw `Error Text.[ Text "unbound word: "; Verbatim name ]
                in
                Simple_type.(TFunc (SError, SError))
          in
          Hashtbl.remove db.types_in_progress name;
          result)
