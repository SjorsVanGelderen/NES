(*
Copyright 2017, Sjors van Gelderen
*)

module Program

open CPU

let version = 1

[<EntryPoint>]
let main args =
    printfn "NES simulator version %A" version

    0
