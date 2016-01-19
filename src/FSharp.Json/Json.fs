[<AutoOpen>]
[<RequireQualifiedAccess>]
module internal FSharp.Json.Utils

open System.Globalization
open JsonParser
open ParserLibrary

type Value = JsonParser.JValue

let serialize (v: Value) = string v
