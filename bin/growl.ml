open Containers
open Growl_core
module String_map = Map.Make (String)
module C = Coalescing.Make ()

let () =
  let db = Db.create () in
  Theory.load_prelude db;

  let fid = Query.FileId "test.grr" in
  Db.store db (Query.SourceText fid)
    (In_channel.with_open_text "test.grr" In_channel.input_all)
    [];
  Db.store db (Query.Manifest ()) [ fid ] [];
  let defs_m = Resolver.ask db (Query.ParsedProgram fid) in
  let defs = Reporting.with_reporting defs_m in
  List.iter
    (fun (d : Ast.def Span.Spanned.t) ->
      let name = d.value.name.value in
      let ty_m = Resolver.ask db (Query.WordType name) in
      let ty = Reporting.with_reporting ty_m in
      if not (Diagnosed.has `Error ty_m) then
        Fmt.epr "The type of %s is %a@." name Type_pp.pp_ty (C.coalesce ty |> Type.simplify_ty))
    defs
