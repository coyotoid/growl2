(* TODO: reporting should show the file it comes from *)

let color_of_severity : Diagnostic.severity -> Fmt.style = function
  | `Error -> `Red
  | `Warning -> `Yellow
  | `Note -> `Cyan

let with_reporting value =
  let value, diagnostics =
    Diagnosed.run value |> Preface.Identity.extract
  in
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
  value
