[<AutoOpen>]
[<RequireQualifiedAccess>]
module internal FSharp.Json.Utils

open System.Globalization
open FSharp.Data

type Value = JsonValue

let parse s = JsonValue.Parse(s, CultureInfo.InvariantCulture)
let serialize (v: Value) = string v
