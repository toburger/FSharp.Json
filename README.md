# FSharp.Json

## Usage example

### Encoding

```fsharp

open FSharp.Json.Encode

encode 4
    jobject [
        "foo", jstring "bar"
        "age", jint 42
        "list", jlist (List.map jint [ 1..10 ])
    ]

```

### Decoding

```fsharp

open FSharp.Json.Decode

let json = "42"
let parsed = decodeString dint json
let result = parsed |> Result.withDefault -1 // simple error handling
printfn "answer = %i" result

```

A more complex example

```fsharp

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

```
