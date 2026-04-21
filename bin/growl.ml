open Containers
open Growl_core
module String_map = Map.Make (String)

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ()
let db = Db.create ()
let () = Theory.load_prelude db
let test_file = "test.grr"
let fid = Query.FileId test_file

let () =
  Db.store db (Query.SourceText fid)
    (In_channel.with_open_text "test.grr" In_channel.input_all)
    [];
  Db.store db (Query.Manifest ()) [ fid ] []

let coalesce t =
  let module C = Coalescing.Make () in
  t |> Compact_type.compact |> Compact_type.simplify |> C.coalesce
  |> Type.simplify_ty

let main () =
  let open Diagnosed.Syntax in
  let program = Resolver.ask db (Query.ParsedProgram fid) in
  if Diagnosed.has `Error program then Diagnosed.return ()
  else
    let* program = program in
    let* main_ty = Resolver.ask db (Query.WordType "main") in
    match Simple_type.is_error main_ty with
    | true -> Diagnosed.return ()
    | false ->
        let* () =
          Diagnosed.throw `Note
            Text.
              [
                Text "the type of ";
                Verbatim "main";
                Text " is: ";
                Any (coalesce main_ty, Type_pp.pp_ty);
              ]
        in
        let stack = Interpret.exec program in
        Fmt.epr "Stk: @[%a@]@." (Fmt.list Interpret.pp_value) stack;
        Diagnosed.return ()

let _ = Reporting.with_reporting (main ())
