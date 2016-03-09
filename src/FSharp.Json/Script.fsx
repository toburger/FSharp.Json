#load @"..\..\paket-files\fsprojects\Chessie\src\Chessie\ErrorHandling.fs"
#load @"..\..\paket-files\mavnn\EmParsec\EmParsec.fs"
#load "Json.fs"
#load "JsonParser.fs"
#load "Encode.fs"
#load "Decode.fs"

open Chessie.ErrorHandling
open FSharp.Json
open FSharp.Json.Decode
open FSharp.Data

decodeValue dint (JNumber 42.)
decodeValue (dnull 42) (JNull)
decodeValue (maybe dint) (JNumber 42.)

type Record =
    { prop1: string
      prop2: int
      prop3: bool }

decodeValue
    (object1 id ("prop1" := dint))
    (JObject (Map.ofList [ "prop1", JNumber 12. ]))

decodeString
    (object3 (fun p1 p2 p3 -> { prop1 = p1; prop2 = p2; prop3 = p3 })
             ("prop1" := dstring)
             ("prop2" := dint)
             ("prop3" := dbool))
             "{ \"prop1\": \"hello\", \"prop2\": 42, \"prop3\": false }"

decodeString
    (at ["person"; "contact"; "email"] dstring)
    "{ \"person\": { \"contact\": { \"email\": \"test@email.com\" } } }"

decodeString
    (dmap dint)
    "{ \"prop1\": 42, \"prop2\": 11, \"prop3\": 999 }"

decodeString
    (at ["number"] dint)
    "{ \"number\": 42 }"

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

open FSharp.Json.Encode

decodeValue
    dint
    (jint (4))

decodeValue
    (dlist dint)
    (jlist ([1..10] |> List.map jint))

module Trial =
    let lift3 f res1 res2 res3 = trial {
        let! r1 = res1
        let! r2 = res2
        let! r3 = res3
        return f r1 r2 r3
    }

let variadic2 (f: 'a -> 'b -> 'c list -> 'value) a b (cs: Decoder<'c>): Decoder<'value> =
    customDecoder (dlist value) (function
        | one::two::rest ->
            let rest' =
                List.map (decodeValue cs) rest
                |> Trial.collect
            Trial.lift3 f
                (decodeValue a one)
                (decodeValue b two)
                rest'
        | _ -> Trial.fail "expecting at least two elements in array")

decodeString
    (variadic2 (fun a b c -> a, b, c) dbool dstring dint)
    "[false, \"test\", 42, 12, 12]"

decodeValue
    (customDecoder dstring (fun v ->
        match System.Int32.TryParse v with
        | true, v -> ok v
        | false, _ -> Trial.fail "not a integer"))
    (jstring "42")

type User =
    { id: int
      username: string option
      email: string option }

let metaDecoder f =
    f <!> ("id" := dint)
      <*> ("username" := maybe dstring)
      <*> ("email" := maybe dstring)

let userDecoder =
    metaDecoder
        (fun id username email -> { id = id; username = username; email = email})

decodeString
    userDecoder
    """{ "id": 42, "username": "john", "email": "email@sample.com" }"""

