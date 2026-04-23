(* TODO: reporting should show the file it comes from *)

module String_map = Map.Make (String)

let color_of_severity : Diagnostic.severity -> Fmt.style = function
  | `Error -> `Red
  | `Warning -> `Yellow
  | `Note -> `Cyan

type code = Error

let code_to_string = function Error -> "error"

let with_reporting diagnosed =
  let value, diagnostics = Diagnosed.run diagnosed in
  List.iter
    (fun diag ->
      match diag.Diagnostic.span with
      | Some span when not Span.(equal span dummy) ->
          Fmt.epr "@[<2>%a:%a: %a: %a@]@."
            (Fmt.styled `Bold Fmt.string)
            span.filename Fmt.text_loc
            ((span.lo.line, span.lo.col - 1), (span.hi.line, span.hi.col - 1))
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
