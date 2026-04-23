exception Fatal of Diagnostic.t list

type _ Effect.t += Emit : Diagnostic.t -> unit Effect.t

let throw ?span severity text =
  Effect.perform (Emit { Diagnostic.severity; text; span })

let fatal ?span severity text =
  raise (Fatal [ { Diagnostic.severity; text; span } ])

let run f =
  let tape = ref [] in
  let result =
    Effect.Deep.match_with f ()
      {
        retc = (fun x -> Some x);
        exnc =
          (function
          | Fatal diag ->
              tape := List.rev_append diag !tape;
              None
          | exn -> raise exn);
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Emit diag ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    tape := diag :: !tape;
                    Effect.Deep.continue k ())
            | _ -> None);
      }
  in
  (result, List.rev !tape)

let adorn ~span f =
  let fill (d : Diagnostic.t) =
    match d.span with None -> { d with span = Some span } | Some _ -> d
  in
  Effect.Deep.match_with f ()
    {
      retc = Fun.id;
      exnc =
        (function
        | Fatal diag -> raise (Fatal (List.map fill diag))
        | exn -> raise exn);
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Emit diag ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (Effect.perform (Emit (fill diag))))
          | _ -> None);
    }

let raise (r, d) =
  let errs = List.filter Diagnostic.is_error d in
  if List.is_empty errs then
    ((match r with Some r -> r | None -> assert false), d)
  else raise (Fatal d)
