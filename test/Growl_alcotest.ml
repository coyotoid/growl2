(** Testable patterns for Alcotest *)

open Growl_core
module String_map = Map.Make (String)

let parse_program str =
  let lexbuf = Lexing.from_string str in
  let result = Parser_intf.parse ~filename:"<test>" lexbuf in
  Diagnosed.run result |> fst

let load_string db fname contents =
  let manifest = Resolver.ask db (Query.Manifest ()) in
  Db.invalidate db (Query.Manifest ());
  let fid = Query.FileId fname in
  Db.store db (Query.SourceText fid) contents [];
  Db.store db (Query.Manifest ()) (fid :: manifest) [];
  fid

let types_of db fid =
  let open Diagnosed.Syntax in
  let program_m = Resolver.ask db (Query.ParsedProgram fid) in
  if Diagnosed.has `Error (Reporting.with_reporting program_m) then
    failwith "error while parsing program"
  else
    let res_m =
      let* program = program_m in
      List.fold_right
        (fun (def : Ast.def Span.Spanned.t) acc ->
          let* acc = acc in
          let* ty = Resolver.ask db (Query.WordType def.value.name.value) in
          Diagnosed.return (String_map.add def.value.name.value ty acc))
        program
        (Diagnosed.return String_map.empty)
    in
    if Diagnosed.has `Error (Reporting.with_reporting res_m) then
      failwith "error during type checking"
    else Diagnosed.run res_m |> fst

let coalesce_ty t =
  let module C = Type_coalescing.Make () in
  t |> Compact_type.compact |> Compact_type.simplify |> C.coalesce
  |> Type.simplify_ty

let ty =
  Alcotest.(
    testable Type_pp.pp_ty (fun (t1 : Type.ty) (t2 : Type.ty) ->
        t1.tag = t2.tag))

let stack =
  Alcotest.(
    testable Type_pp.pp_stack (fun (s1 : Type.stack) (s2 : Type.stack) ->
        s1.tag = s2.tag))

let setup_db () =
  let db = Db.create () in
  Db.store db (Query.Manifest ()) [] [];
  Theory.load_prelude db;
  db

let ( &> ) = Fun.flip Growl_core.Type.scons
let ( => ) = Growl_core.Type.tfunc
