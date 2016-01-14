module FSharp.Json.Decode.Tests

open FSharp.Json.Decode
open NUnit.Framework

let (|Result|) res =
    match res with
    | Ok v -> v
    | Err err -> failwithf "unexpected: %s" err

[<Test>]
let ``returns "hello world"`` () =
    let (Result result) = decodeString dstring "\"hello world\""
    Assert.AreEqual("hello world", result)

[<Test>]
let ``returns 42`` () =
  let (Result result) = decodeString dint "42"
  Assert.AreEqual(42, result)

[<Test>]
let ``returns 1.23123`` () =
    let (Result result) = decodeString dfloat "1.23123"
    Assert.AreEqual(1.23123, result)

[<Test>]
let ``returns true`` () =
    let (Result result) = decodeString dbool "true"
    Assert.AreEqual(true, result)

[<Test>]
let ``returns false`` () =
    let (Result result) = decodeString dbool "false"
    Assert.AreEqual(false, result)

[<Test>]
let ``returns 42 if null`` () =
    let (Result result) = decodeString (dnull 42) ""
    Assert.AreEqual(42, result)

[<Test>]
let ``returns maybe 42`` () =
    let (Result result) = decodeString (maybe dint) "42"
    Assert.AreEqual(Some 42, result)
    let (Result result) = decodeString (maybe dint) ""
    Assert.AreEqual(None, result)


