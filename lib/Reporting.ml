(* TODO: reporting should show the file it comes from *)

module String_map = Map.Make (String)

let color_of_severity : Diagnostic.severity -> Fmt.style = function
  | `Error -> `Red
  | `Warning -> `Yellow
  | `Note -> `Cyan

type code = Error

let code_to_string = function Error -> "error"

let with_grace_reporting db diagnosed =
  let value, diagnostics = Diagnosed.run diagnosed in
  let sources =
    List.map (fun (d : Diagnostic.t) -> d.span) diagnostics
    |> List.sort_uniq (fun (s1 : Span.t option) (s2 : Span.t option) ->
        match (s1, s2) with
        | None, None -> 0
        | None, _ -> -1
        | _, None -> 1
        | Some s1, Some s2 -> String.compare s1.filename s2.filename)
    |> List.filter_map (Option.map (fun (s : Span.t) -> s.filename))
  in
  let sources =
    List.map
      (fun fname ->
        let fid = Query.FileId fname in
        let content = Resolver.ask db (Query.SourceText fid) in
        let source : Grace.Source.t = `String { name = Some fname; content } in
        (fname, source))
      sources
    |> String_map.of_list
  in
  let labels =
    List.filter_map
      (fun (d : Diagnostic.t) ->
        match d.span with
        | None -> None
        | Some span ->
            let range =
              Grace.Range.create
                ~source:(String_map.find span.filename sources)
                (Grace.Byte_index.of_int span.lo.byte)
                (Grace.Byte_index.of_int span.hi.byte)
            in
            if Diagnostic.equal_severity d.severity `Error then
              Some
                (Grace.Diagnostic.Label.primaryf ~range "%s" (Text.show d.text))
            else
              Some
                (Grace.Diagnostic.Label.secondaryf ~range "%s"
                   (Text.show d.text)))
      diagnostics
  in
  let diagnostic =
    Grace.Diagnostic.createf ~code:Error ~labels Grace.Diagnostic.Severity.Error
      "error"
  in
  Fmt.epr "%a@."
    Grace_ansi_renderer.(
      pp_diagnostic ~code_to_string ~config:Grace_ansi_renderer.Config.default)
    diagnostic;
  diagnosed

let with_reporting diagnosed =
  let value, diagnostics = Diagnosed.run diagnosed in
  List.iter
    (fun diag ->
      match diag.Diagnostic.span with
      | Some span when not Span.(equal span dummy) ->
          Fmt.epr "@[<2>%a:%a: %a: %a@]@."
            (Fmt.styled `Bold Fmt.string)
            span.filename Fmt.text_loc
            ((span.lo.line, span.lo.col), (span.hi.line, span.hi.col))
            (Fmt.styled `Bold
               (Fmt.styled
                  (color_of_severity diag.Diagnostic.severity)
                  Fmt.string))
            (Diagnostic.string_of_severity diag.Diagnostic.severity)
            Text.pp diag.text
      | _ ->
          Fmt.epr "@[<2>%a: %a@]@."
            (Fmt.styled `Bold
               (Fmt.styled
                  (color_of_severity diag.Diagnostic.severity)
                  Fmt.string))
            (Diagnostic.string_of_severity diag.Diagnostic.severity)
            Text.pp diag.text)
    diagnostics;
  diagnosed
