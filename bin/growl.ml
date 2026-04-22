open Cmdliner
open Growl_core

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ()

let file =
  let doc = "The source file to process." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let setup_db file =
  let db = Db.create () in
  Theory.load_prelude db;
  let fid = Query.FileId file in
  Db.store db (Query.SourceText fid)
    (In_channel.with_open_text file In_channel.input_all)
    [];
  Db.store db (Query.Manifest ()) [ fid ] [];
  (db, fid)

let coalesce_ty t =
  let module C = Type_coalescing.Make () in
  t |> Compact_type.compact |> Compact_type.simplify |> C.coalesce
  |> Type.simplify_ty

let exec_cmd =
  let run file =
    let db, fid = setup_db file in
    let open Diagnosed.Syntax in
    let program = Resolver.ask db (Query.ParsedProgram fid) in
    if Diagnosed.has `Error program then Diagnosed.return ()
    else
      let* program = program in
      let* types =
        List.fold_right
          (fun (def : Ast.def Span.Spanned.t) acc ->
            let* acc = acc in
            let* ty = Resolver.ask db (Query.WordType def.value.name.value) in
            Diagnosed.return ((def.value.name.value, ty) :: acc))
          program (Diagnosed.return [])
      in
      let* main_ty = Resolver.ask db (Query.WordType "main") in
      match Simple_type.is_error main_ty with
      | true -> Diagnosed.return ()
      | false ->
          let stack = Interpret.exec program in
          Fmt.epr "Resulting stack: @[%a@]@."
            (Fmt.brackets (Fmt.list ~sep:Fmt.sp Interpret.pp_value))
            (List.rev stack);
          Diagnosed.return ()
  in
  let run file =
    Reporting.with_reporting (run file) |> Diagnosed.run |> Pair.fst
  in
  let doc = "Type-check and run a source file." in
  Cmd.v (Cmd.info "exec" ~doc) Term.(const run $ file)

let check_cmd =
  let run file =
    let db, fid = setup_db file in
    let open Diagnosed.Syntax in
    let program = Resolver.ask db (Query.ParsedProgram fid) in
    if Diagnosed.has `Error program then Diagnosed.return ()
    else
      let* program = program in
      let* types =
        List.fold_right
          (fun (def : Ast.def Span.Spanned.t) acc ->
            let* acc = acc in
            let* ty = Resolver.ask db (Query.WordType def.value.name.value) in
            Diagnosed.return ((def.value.name.value, ty) :: acc))
          program (Diagnosed.return [])
      in
      Diagnosed.return
      @@ List.iter
           (fun (name, ty) ->
             Fmt.pr "%a@." Text.pp
               Text.
                 [ Text name; Text " :: "; Any (coalesce_ty ty, Type_pp.pp_ty) ])
           types
  in
  let run file =
    Reporting.with_reporting (run file) |> Diagnosed.run |> Pair.fst
  in
  let doc = "Type-check and print a source file's type definitions." in
  Cmd.v (Cmd.info "check" ~doc) Term.(const run $ file)

let cmd =
  let doc = "an experimental stack-based language" in
  Cmd.group (Cmd.info "growl" ~doc) [ exec_cmd; check_cmd ]

let () = exit (Cmd.eval cmd)
