open Containers

type pos = { line : int; col : int } [@@deriving eq]
type t = { filename : string; lo : pos; hi : pos } [@@deriving eq]

let dummy : t =
  let dummy_pos = { line = 0; col = 0 } in
  { filename = ""; lo = dummy_pos; hi = dummy_pos }

let merge : t -> t -> t =
 fun a b ->
  assert (String.equal a.filename b.filename);
  let before a b = a.line < b.line || (a.line = b.line && a.col <= b.col) in
  {
    filename = a.filename;
    lo = (if before a.lo b.lo then a.lo else b.lo);
    hi = (if before a.hi b.hi then b.hi else a.hi);
  }

module Spanned = struct
  type nonrec 'a t = { value : 'a; span : t }
end
