#r @"..\..\packages\Newtonsoft.Json\lib\net40\Newtonsoft.Json.dll"
#load "Result.fs"
#load "Encode.fs"
#load "Decode.fs"

open FSharp.Json.Decode

decodeString dint "42"
decodeString (dnull 42) ""
decodeString (maybe dint) "42"

type Record =
    { prop1: string
      prop2: int
      prop3: bool }

decodeString
    (object3 (fun p1 p2 p3 -> { prop1 = p1; prop2 = p2; prop3 = p3 })
             ("prop1" := dstring)
             ("prop2" := dint)
             ("prop3" := dbool))
             "{ prop1: \"hello\", prop2: 42, prop3: false }"

decodeString
    (at ["person"; "contact"; "email"] dstring)
    "{ person: { contact: { email: \"test@email.com\" } } }"

decodeString
    (dmap dint)
    "{ prop1: 42, prop2: 11, prop3: 999 }"

decodeString
    (at ["number"] dint)
    "{ number: 42 }"
