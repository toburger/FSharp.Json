(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Json"

(**
Introducing your project
========================

Say more

*)

#r "FSharp.Json.dll"
open FSharp.Json.Decode

type Record =
    { prop1: string
      prop2: int
      prop3: bool }

let parsed =
    decodeString
        (object3 (fun p1 p2 p3 -> { prop1 = p1; prop2 = p2; prop3 = p3 })
                 ("prop1" := dstring)
                 ("prop2" := dint)
                 ("prop3" := dbool))
                 "{ prop1: \"hello\", prop2: 42, prop3: false }"

printfn "%A" parsed

(**
Some more info
*)
