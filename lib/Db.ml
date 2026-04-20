module Key = struct
  type t = Pack : 'a Query.t -> t

  let equal : t -> t -> bool =
   fun (Pack q1) (Pack q2) -> Query.equal q1 q2 |> Option.is_some

  let hash : t -> int = fun (Pack q) -> Query.hash q
end

module Table = Hashtbl.Make (Key)

type entry = Entry : 'a Query.t * 'a * Key.t list -> entry

type t = {
  cache : entry Table.t;
  rdeps : Key.t list Table.t;
  mutable current_deps : Key.t list;
  (* for recursive calls to work *)
  types_in_progress : (string, Simple_type.ty * bool ref) Hashtbl.t;
}

let create : unit -> t =
 fun () ->
  let module I = Inference.Make () in
  {
    cache = Table.create 64;
    rdeps = Table.create 64;
    current_deps = [];
    types_in_progress = Hashtbl.create 8;
  }

let find : type a. t -> a Query.t -> a option =
 fun db q ->
  match Table.find_opt db.cache (Key.Pack q) with
  | None -> None
  | Some (Entry (q', v, _deps)) -> (
      match Query.equal q q' with Some Refl -> Some v | None -> assert false)

let store : type a. t -> a Query.t -> a -> Key.t list -> unit =
 fun db q v deps ->
  Table.replace db.cache (Key.Pack q) (Entry (q, v, deps));
  List.iter
    (fun dep ->
      let rev = Table.find_opt db.rdeps dep |> Option.value ~default:[] in
      Table.replace db.rdeps dep (Key.Pack q :: rev))
    deps

let invalidate : type a. t -> a Query.t -> unit =
  let rec invalidate_key db key =
    Table.remove db.cache key;
    match Table.find_opt db.rdeps key with
    | None -> ()
    | Some dependents ->
        Table.remove db.rdeps key;
        List.iter (invalidate_key db) dependents
  in
  fun db q -> invalidate_key db (Key.Pack q)
