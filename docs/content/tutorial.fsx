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

let json =
    encode true <|
        jobject [
            "foo", jstring "bar"
            "age", jint 42
            "list", jlist (List.map jint [ 1..10 ])
        ]

(*** include-value:json ***)

(**

### Decoder

#### Decode a Record

*)

open FSharp.Json.Decode

type Record =
    { prop1: string
      prop2: int
      prop3: bool }

let record =
    decodeString
        (object3 (fun p1 p2 p3 -> { prop1 = p1; prop2 = p2; prop3 = p3 })
                 ("prop1" := dstring)
                 ("prop2" := dint)
                 ("prop3" := dbool))
        "{ \"prop1\": \"hello\", \"prop2\": 42, \"prop3\": false }"

(*** include-value:record ***)

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
        object2 (fun w h -> Rectangle(w, h))
                ("width" := dfloat)
                ("height" := dfloat)
    | "circle" ->
        object1 Circle ("radius" := dfloat)
    | _ -> fail (tag + " is not a recognized tag for shape")

let shape =
    ("tag" := dstring) >>= shapeInfo

(** Rectangle *)

let rectangle =
    decodeString
        shape
        """{ "tag": "rectangle", "width": 100, "height": 40 }"""

(*** include-value:rectangle ***)

(** Circle *)

let circle =
    decodeString
        shape
        """{ "tag": "circle", "radius": 90 }"""

(*** include-value:circle ***)

(** Bad *)

let bad =
    decodeString
        shape
        """{ "tag": "other" }"""

(*** include-value:bad ***)

(**

#### Decode oddly shaped values

*)

open FSharp.Json.Decode

type Person =
    { name: string
      age: int
      profession: string option }

let dperson =
    object3 (fun n a p -> { name = n; age = a; profession = p })
            ("name" := dstring)
            ("age" := dint)
            (maybe ("profession" := dstring))

(** Profession: Some "plumber" *)
let tom = decodeString dperson """{ "name": "Tom", "age": 31, "profession": "plumber" }"""

(*** include-value:tom ***)

(** Profession: None *)
let sue = decodeString dperson """{ "name": "Sue", "age": 42 }"""
let amy = decodeString dperson """{ "name": "Amy", "age": 27, "profession": null }"""
let joe = decodeString dperson """{ "name": "Joe", "age": 36, "profession": ["something", "unexpected"] }"""

(*** include-value:sue ***)
(*** include-value:amy ***)
(*** include-value:joe ***)
