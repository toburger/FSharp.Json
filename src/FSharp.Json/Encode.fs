module FSharp.Json.Encode

open FSharp.Data
open System.Globalization
open JsonParser

let encode (indent: bool) (value: JValue) =
    "todo"

//    use sw = new System.IO.StringWriter(CultureInfo.InvariantCulture)
//    let saveOptions =
//        if indent
//        then JsonSaveOptions.None
//        else JsonSaveOptions.DisableFormatting
//    value.WriteTo(sw, saveOptions)
//    sw.ToString()

let jstring (value: string): JValue = JString value

let jint (value: int): JValue = JNumber (float value)

let jfloat (value: float): JValue = JNumber (float value)

let jbool (value: bool): JValue = JBool value

let jnull: JValue = JNull

let jobject (props: (string * JValue) seq) : JValue =
    props
    |> Map.ofSeq
    |> JObject

let jlist (list: JValue seq) : JValue =
    JArray (Seq.toList list)
