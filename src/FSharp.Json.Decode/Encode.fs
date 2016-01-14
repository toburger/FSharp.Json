module FSharp.Json.Encode

open Newtonsoft.Json

type Value = Newtonsoft.Json.Linq.JToken

let encode indentLevel value =
    use sw = new System.IO.StringWriter()
    use writer = new JsonTextWriter(sw, Indentation = indentLevel)
    let serializer = JsonSerializer()
    serializer.Serialize(writer, value)
    sw.ToString()

//let identity (value: obj): Value = value
