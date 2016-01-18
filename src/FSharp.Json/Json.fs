[<AutoOpen>]
[<RequireQualifiedAccess>]
module internal FSharp.Json.Utils

open Newtonsoft.Json

type Value = Linq.JToken

let parse s = JsonConvert.DeserializeObject<Value> s
let serialize (o: obj) = JsonConvert.SerializeObject o
