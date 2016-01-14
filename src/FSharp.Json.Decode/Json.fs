[<AutoOpen>]
[<RequireQualifiedAccess>]
module internal FSharp.Json.Utils

open Newtonsoft.Json

let parse s = JsonConvert.DeserializeObject<Newtonsoft.Json.Linq.JToken> s
let serialize (o: obj) = JsonConvert.SerializeObject o
