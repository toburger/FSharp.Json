module FSharp.Json.Tests.Encode

open FSharp.Json.Encode
open NUnit.Framework

let (==) (expected: string) value =
    Assert.AreEqual(expected, encode 0 value)

[<Test>]
let ``returns json`` () =
    "\"foo\"" == jstring "foo"
    "42" == jint 42
    "1.24" == jfloat 1.24
    "true" == jbool true
    "null" == jnull
    "{\"foo\":\"bar\"}" == jobject [ "foo", jstring "bar" ]
    "[1,2,3,4,5,6,7,8,9,10]" == jlist (List.map jint [ 1..10 ])
