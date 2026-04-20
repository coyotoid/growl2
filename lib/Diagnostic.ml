type severity = [ `Error | `Warning | `Note ] [@@deriving eq]
type t = { severity : severity; text : Text.t; span : Span.t option }

let string_of_severity : severity -> string = function
  | `Error -> "error"
  | `Warning -> "warning"
  | `Note -> "note"

let is_error : t -> bool =
 fun d -> match d.severity with `Error -> true | _ -> false

let is_warning : t -> bool =
 fun d -> match d.severity with `Warning -> true | _ -> false

let is_note : t -> bool =
 fun d -> match d.severity with `Note -> true | _ -> false
