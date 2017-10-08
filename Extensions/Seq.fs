module internal Elmish.Extensions.Seq

let bindOption mapper sequence =
    seq {
        for v in sequence do
            match mapper v with
            | Some r -> yield r
            | None -> ()
    }
