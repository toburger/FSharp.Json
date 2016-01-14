module FSharp.Json.Encode

open Newtonsoft.Json

type Value = Newtonsoft.Json.Linq.JToken

let encode indentLevel (value: Value) =
    use sw = new System.IO.StringWriter()
    use writer = new JsonTextWriter(sw, Indentation = indentLevel)
    if indentLevel > 0 then writer.Formatting <- Formatting.Indented
    let serializer = JsonSerializer()
    serializer.Serialize(writer, value)
    sw.ToString()

let identity<'a> (value: 'a) = unbox (Linq.JValue(value))

let jstring (value: string): Value = identity value

let jint (value: int): Value = identity value

let jfloat (value: float): Value = identity value

let jbool (value: bool): Value = identity value

let jnull: Value = identity (null: obj)

let jobject (props: (string * #Value) list) : #Value =
    props
    |> List.map (fun (n, v) -> Linq.JProperty(n, v))
    |> Linq.JObject
    |> unbox

let jlist (list: Value list) : Value =
    unbox (Linq.JArray(list))
