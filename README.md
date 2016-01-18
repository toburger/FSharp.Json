# FSharp.Json

## Description

A simple port of [elm](http://elm-lang.org/)'s Json [encoding](http://package.elm-lang.org/packages/elm-lang/core/1.1.0/Json-Encode) and [decoding](http://package.elm-lang.org/packages/elm-lang/core/1.1.0/Json-Decode) library and a simple to use alternative to [Chiron](https://github.com/xyncro/chiron) and [Fleece](https://github.com/mausch/Fleece).

Documentation for the Decoding API can be found [in the elm packages documentation](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Json-Decode).  
The Encoding API is documented [here](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Json-Encode).

## Usage example

### Encoding

```fsharp

open FSharp.Json.Encode

encode true <|
    jobject [
        "foo", jstring "bar"
        "age", jint 42
        "list", jlist (List.map jint [ 1..10 ])
    ]

```

### Decoding

```fsharp

open Chessie.ErrorHandling
open FSharp.Json.Decode

let json = "[42, 9]"
let parsed = decodeString (dlist dint) json
let result = parsed |> Trial.returnOrFail
printfn "answer = %A" result

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
