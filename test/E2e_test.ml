(* Testing the query engine *)

open Growl_alcotest

let test_simple () =
  let db = setup_db () in
  let fid =
    load_string db "test.grr"
      {|
        def test {
          1 2 3 [+] dip
        }  
      |}
  in
  let types = types_of db fid in
  match String_map.find_opt "test" types with
  | None -> Alcotest.fail "word test doesn't exist"
  | Some s ->
      Alcotest.check ty "types are equal" (coalesce_ty s)
        Growl_core.Type.(svar "r" => (svar "r" &> tprim `Int &> tprim `Nat))

let test_recursive () =
  let db = setup_db () in
  let fid =
    load_string db "test.grr"
      {|
        def ifte { choose call }
        def fact {
          dup 2 < [drop 1] [dup 1 - fact *] ifte
        }
      |}
  in
  let types = types_of db fid in
  match String_map.find_opt "fact" types with
  | None -> Alcotest.fail "word fact doesn't exist"
  | Some s ->
      Alcotest.check ty "types are equal" (coalesce_ty s)
        Growl_core.Type.(svar "r" &> tprim `Int => (svar "r" &> tprim `Int))

let test_mutually_recursive () =
  let db = setup_db () in
  let fid =
    load_string db "test.grr"
      {|
        def ifte { choose call }
        def even { dup 0 = [drop true] [1 - odd] ifte }
        def odd { even not }
      |}
  in
  let types = types_of db fid in
  let assert_ty word =
    match String_map.find_opt word types with
    | None -> Alcotest.fail ("word " ^ word ^ " doesn't exist")
    | Some s ->
        Alcotest.check ty "types are equal" (coalesce_ty s)
          Growl_core.Type.(svar "r" &> tprim `Int => (svar "r" &> tprim `Bool))
  in
  assert_ty "even";
  assert_ty "odd"

let test_non_terminating () =
  let db = setup_db () in
  let fid =
    load_string db "test.grr"
      {|
        def forever { forever }
        def forever-2 { [dup call] dup call }
      |}
  in
  let types = types_of db fid in
  let assert_ty word =
    match String_map.find_opt word types with
    | None -> Alcotest.fail ("word " ^ word ^ " doesn't exist")
    | Some s ->
        Alcotest.check ty "types are equal" (coalesce_ty s)
          Growl_core.Type.(stop => sbot)
  in
  assert_ty "forever";
  assert_ty "forever-2"

let () =
  Alcotest.(
    run "Growl"
      [
        ( "E2E",
          [
            test_case "simple inference" `Quick test_simple;
            test_case "recursive inference (factorial)" `Quick test_recursive;
            test_case "mutually recursive inference (even/odd)" `Quick
              test_mutually_recursive;
            test_case "non-terminating inference (forever)" `Quick test_non_terminating
          ] );
      ])
