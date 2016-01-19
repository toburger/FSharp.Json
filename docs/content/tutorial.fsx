(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Json"
#r "FSharp.Json.dll"
#r "Newtonsoft.Json"

(**
Tutorial
========================

### Encoder

*)

open FSharp.Json.Encode

encode true <|
    jobject [
        "foo", jstring "bar"
        "age", jint 42
        "list", jlist (List.map jint [ 1..10 ])
    ]

(**

### Decoder

#### Decode a Record

*)

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
                 "{ \"prop1\": \"hello\", \"prop2\": 42, \"prop3\": false }"

printfn "%A" parsed

(**

#### Decode a discriminated Union

*)

open FSharp.Json.Decode

type Shape =
    | Rectangle of float * float
    | Circle of float

let shapeInfo tag =
    match tag with
    | "rectangle" ->
        object2 (fun w h -> Rectangle(w, h)) ("width" := dfloat) ("height" := dfloat)
    | "circle" ->
        object1 Circle ("radius" := dfloat)
    | _ -> fail (tag + " is not a recognized tag for shape")

let shape =
    ("tag" := dstring) >>= shapeInfo

decodeString shape
    """{ "tag": "rectangle", "width": 100, "height": 40 }"""
decodeString shape
    """{ "tag": "circle", "radius": 90 }"""
decodeString shape
    """{ "tag": "other" }"""
