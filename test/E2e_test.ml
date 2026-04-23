(* Testing the query engine *)

open Growl_alcotest

let assert_type types name ty =
  match String_map.find_opt name types with
  | None -> Alcotest.fail (Printf.sprintf "word `%s` doesn't exist" name)
  | Some ty' ->
      Alcotest.check Growl_alcotest.ty "types are not equal" (coalesce_ty ty')
        ty

let test_simple () =
  let db = setup_db () in
  let fid =
    load_string db "test.grr"
      {|
        def ifte { choose call }
      |}
  in
  let types = types_of db fid in
  let arr = Growl_core.Type.(svar "r" => svar "s") in
  assert_type types "ifte"
    Growl_core.Type.(svar "r" &> tprim `Bool &> arr &> arr => svar "s")

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
  assert_type types "fact"
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
  let expected =
    Growl_core.Type.(svar "r" &> tprim `Int => (svar "r" &> tprim `Bool))
  in
  assert_type types "even" expected;
  assert_type types "odd" expected

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
  let expected = Growl_core.Type.(stop => sbot) in
  assert_type types "forever" expected;
  assert_type types "forever-2" expected

let () =
  Alcotest.(
    run "Growl_core"
      [
        ( "end-to-end",
          [
            test_case "simple inference" `Quick test_simple;
            test_case "recursive inference (factorial)" `Quick test_recursive;
            test_case "mutually recursive inference (even/odd)" `Quick
              test_mutually_recursive;
            test_case "non-terminating inference (forever)" `Quick
              test_non_terminating;
          ] );
      ])
