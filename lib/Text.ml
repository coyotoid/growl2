open Containers

type node =
  | Text : string -> node
  | Verbatim : string -> node
  | Any : ('a * 'a Fmt.t) -> node
  | Break : node

type t = node list

let sectionize x =
  let lst = ref [] in
  let inner = ref [] in
  List.iter
    (fun e ->
      match e with
      | (Text _ | Verbatim _ | Any _) as n -> inner := n :: !inner
      | Break ->
          lst := List.rev !inner :: !lst;
          inner := [])
    x;
  if not (List.is_empty !inner) then lst := List.rev !inner :: !lst;
  List.rev !lst

let pp_node : node Fmt.t =
  let open Fmt in
  fun ppf t ->
    match t with
    | Text t -> string ppf t
    | Verbatim t ->
        (const string "`" ++ styled `Bold string ++ const string "`") ppf t
    | Any (a, fmt) ->
        (const string "`" ++ styled `Bold fmt ++ const string "`") ppf a
    | Break ->
        Format.pp_force_newline ppf ();
        Format.pp_force_newline ppf ()

let pp : t Fmt.t = Fmt.(list ~sep:nop pp_node)
let show : t -> string = Fmt.to_to_string pp

(* HTML output functions *)

let pp_html_escaped : string Fmt.t =
  let open Fmt in
  fun ppf str ->
    String.iter
      (fun c ->
        match c with
        | '<' -> string ppf "&lt;"
        | '>' -> string ppf "&gt;"
        | '&' -> string ppf "&amp;"
        | _ -> char ppf c)
      str

let pp_html_of_node : node Fmt.t =
  let open Fmt in
  fun ppf -> function
    | Text t -> pp_html_escaped ppf t
    | Verbatim c -> (any "<code>" ++ pp_html_escaped ++ any "</code>") ppf c
    | Any (v, fmt) ->
        let str = str "%a" fmt v in
        string ppf "<code>";
        pp_html_escaped ppf str;
        string ppf "</code>"
    | Break -> string ppf "<br><br>"

let pp_html : t Fmt.t =
  let sectionize x =
    let lst = ref [] in
    let inner = ref [] in
    List.iter
      (fun e ->
        match e with
        | (Text _ | Verbatim _ | Any _) as n -> inner := n :: !inner
        | Break ->
            lst := List.rev !inner :: !lst;
            inner := [])
      x;
    if not (List.is_empty !inner) then lst := List.rev !inner :: !lst;
    List.rev !lst
  in
  fun ppf t ->
    List.iter
      (fun section ->
        Fmt.string ppf "<p>";
        List.iter (pp_html_of_node ppf) section;
        Fmt.string ppf "<p>")
      (sectionize t)

let html : t -> string = Fmt.to_to_string pp_html
