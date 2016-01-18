module FSharp.Json.Encode

open FSharp.Data
open System.Globalization

type Value = Utils.Value

let encode indent (value: Value) =
    use sw = new System.IO.StringWriter(CultureInfo.InvariantCulture)
    let saveOptions =
        if indent
        then JsonSaveOptions.None
        else JsonSaveOptions.DisableFormatting
    value.WriteTo(sw, saveOptions)
    sw.ToString()

let jstring (value: string): Value = JsonValue.String value

let jint (value: int): Value = JsonValue.Number (decimal value)

let jfloat (value: float): Value = JsonValue.Number (decimal value)

let jbool (value: bool): Value = JsonValue.Boolean value

let jnull: Value = JsonValue.Null

let jobject (props: (string * Value) seq) : Value =
    props
    |> Seq.toArray
    |> JsonValue.Record

let jlist (list: Value seq) : Value =
    JsonValue.Array (Seq.toArray list)
