#r @"..\..\packages\Newtonsoft.Json\lib\net40\Newtonsoft.Json.dll"
#load @"..\..\paket-files\fsprojects\Chessie\src\Chessie\ErrorHandling.fs"
#load "Json.fs"
#load "Encode.fs"
#load "Decode.fs"

open FSharp.Json.Decode
open Chessie.ErrorHandling

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

decodeString
    (tuple1 id dint)
    "[42]"

decodeString
    (tuple2 (fun s i -> s, i) dstring dint)
    "[\"foo\", 42]"

decodeString
    (tuple3 (fun s i s2 -> s, i, s2) dstring dint dstring)
    "[\"foo\", 42, \"baz\"]"

decodeString
    (tuple4 (fun s i s2 b -> s, i, s2, b) dstring dint dstring dbool)
    "[\"foo\", 42, \"baz\", false]"

decodeValue
    dint
    (Newtonsoft.Json.Linq.JValue(4))

decodeValue
    (list dint)
    (Newtonsoft.Json.Linq.JArray([1..10] |> List.map Newtonsoft.Json.Linq.JValue))

//let variadic2 (f: 'a -> 'b -> 'c list -> 'value) a b (cs: Decoder<'c>): Decoder<'value> =
//    customDecoder (list value) (function
//        | one::two::rest ->
//            let rest' =
//                List.map (decodeValue cs) rest
//                |> Result.transform
//                |> Result.mapError (fun ls -> sprintf "%A" ls)
//            Result.map3 f
//                (decodeValue a one)
//                (decodeValue b two)
//                rest'
//        | _ -> Err "expecting at least two elements in array")
//
//decodeString
//    (variadic2 (fun a b c -> a, b, c) dbool dstring dint)
//    "[false, \"test\", 42, 12, 12]"

decodeString
    (customDecoder dstring (fun v ->
        match System.Int32.TryParse v with
        | true, v -> ok v
        | false, _ -> fail "not a integer"))
    "\"42\""

