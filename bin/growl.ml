open Containers
open Growl_core
module String_map = Map.Make (String)

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ()
let db = Db.create ()
let () = Theory.load_prelude db
let test_file = if Array.length Sys.argv > 1 then Sys.argv.(1) else "test.grr"
let fid = Query.FileId test_file

let () =
  Db.store db (Query.SourceText fid)
    (In_channel.with_open_text test_file In_channel.input_all)
    [];
  Db.store db (Query.Manifest ()) [ fid ] []

let print_text t = Fmt.pr "%a@." Text.pp t
let prerr_text t = Fmt.epr "%a@." Text.pp t

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

    (* List the type of all definitions *)
    let* types =
      List.fold_right
        (fun (def : Ast.def Span.Spanned.t) acc ->
          let* acc = acc in
          let* ty = Resolver.ask db (Query.WordType def.value.name.value) in
          Diagnosed.return ((def.value.name.value, ty) :: acc))
        program (Diagnosed.return [])
    in

    List.iter
      (fun (name, ty) ->
        prerr_text
          Text.
            [
              Text "The type of ";
              Verbatim name;
              Text " is ";
              Any (coalesce ty, Type_pp.pp_ty);
            ])
      types;

    let* main_ty = Resolver.ask db (Query.WordType "main") in
    match Simple_type.is_error main_ty with
    | true -> Diagnosed.return ()
    | false ->
        let stack = Interpret.exec program in
        Fmt.epr "Resulting stack: @[%a@]@."
          (Fmt.brackets (Fmt.list ~sep:Fmt.sp Interpret.pp_value))
          stack;
        Diagnosed.return ()

let _ = Reporting.with_reporting (main ())
