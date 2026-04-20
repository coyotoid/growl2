include
  Preface.Make.Writer.Over_monad
    (Preface.Identity.Monad)
    (Preface.List.Monoid (struct
      type t = Diagnostic.t
    end))

let throw ?span severity text = tell [ { severity; text; span } ]

let has severity m =
  exec m |> Preface.Identity.extract
  |> List.exists (fun d ->
      Diagnostic.equal_severity d.Diagnostic.severity severity)

let adorn ~span m =
  let fill (d : Diagnostic.t) =
    match d.span with None -> { d with span } | Some _ -> d
  in
  censor (List.map fill) m
