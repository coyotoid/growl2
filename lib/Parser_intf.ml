module I = Parser.MenhirInterpreter

let current_span filename (lexbuf : Lexing.lexbuf) =
  let to_pos (p : Lexing.position) =
    Span.{ line = p.pos_lnum; col = p.pos_cnum - p.pos_bol }
  in
  Span.
    { filename; lo = to_pos lexbuf.lex_start_p; hi = to_pos lexbuf.lex_curr_p }

let parse ~filename lexbuf =
  Lexing.set_filename lexbuf filename;
  let next () =
    let tok = Lexer.token lexbuf in
    (tok, lexbuf.lex_start_p, lexbuf.lex_curr_p)
  in
  let last_token = ref (Parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos) in
  let open Diagnosed in
  let rec step checkpoint =
    match checkpoint with
    | I.InputNeeded _ -> (
        let ((tok, _, _) as t) = next () in
        last_token := t;
        match I.offer checkpoint t with
        | I.HandlingError _ as err_cp ->
            let span = current_span filename lexbuf in
            throw ~span `Error Text.[ Text "syntax error" ] >>= fun () ->
            retry t (I.resume err_cp)
        | next_cp -> step next_cp)
    | I.Shifting _ | I.AboutToReduce _ -> step (I.resume checkpoint)
    | I.Accepted tree -> return tree
    | I.Rejected -> return []
    | I.HandlingError _ ->
        let span = current_span filename lexbuf in
        let* () = throw ~span `Error Text.[ Text "syntax error" ] in
        retry !last_token (I.resume checkpoint)
  and retry pending checkpoint =
    match checkpoint with
    | I.Rejected -> return []
    | I.Accepted tree -> return tree
    | I.Shifting _ | I.AboutToReduce _ -> retry pending (I.resume checkpoint)
    | I.HandlingError _ -> retry pending (I.resume checkpoint)
    | I.InputNeeded _ -> (
        match I.offer checkpoint pending with
        | I.HandlingError _ as hcp -> (
            let tok, _, _ = pending in
            match tok with
            | Parser.EOF -> return []
            | _ ->
                let rec resolve = function
                  | I.Shifting _ as cp -> step (I.resume cp)
                  | I.Rejected -> return []
                  | I.HandlingError _ as cp -> resolve (I.resume cp)
                  | cp -> step cp
                in
                resolve (I.resume hcp))
        | next_cp -> (
            let tok, _, _ = pending in
            match tok with Parser.EOF -> return [] | _ -> step next_cp))
  in
  step (Parser.Incremental.program lexbuf.lex_curr_p)

let parse_string ?(filename = "<string>") s =
  parse ~filename (Lexing.from_string s)

let parse_channel ?(filename = "<channel>") c =
  parse ~filename (Lexing.from_channel c)
