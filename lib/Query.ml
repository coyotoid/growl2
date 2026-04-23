open Containers

type file_id = FileId of string [@@deriving eq]

type _ t =
  | Manifest : unit -> file_id list t
  | SourceText : file_id -> string t
  | ParsedProgram : file_id -> Ast.program t
  | SCCs : unit -> string list list t
  | WordDef : string -> Ast.def Span.Spanned.t option t
  | WordType : string -> Simple_type.ty t

type ('a, 'b) eq = Refl : ('a, 'a) eq

let equal : type a b. a t -> b t -> (a, b) eq option =
 fun q1 q2 ->
  match (q1, q2) with
  | Manifest _, Manifest _ -> Some Refl
  | SourceText i, SourceText j when equal_file_id i j -> Some Refl
  | ParsedProgram i, ParsedProgram j when equal_file_id i j -> Some Refl
  | SCCs (), SCCs () -> Some Refl
  | WordDef i, WordDef j when String.equal i j -> Some Refl
  | WordType i, WordType j when String.equal i j -> Some Refl
  | _ -> None

let hash : type a. a t -> int = function
  | Manifest () -> Hashtbl.hash `Manifest
  | SourceText i -> Hashtbl.hash (`SourceText i)
  | ParsedProgram i -> Hashtbl.hash (`ParsedProgram i)
  | SCCs () -> Hashtbl.hash `SCCs
  | WordDef name -> Hashtbl.hash (`WordDef name)
  | WordType name -> Hashtbl.hash (`WordType name)
