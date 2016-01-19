module FSharp.Json.Encode

open System.Globalization
open JsonParser

type Value = Utils.Value

let encode (indent: bool) (value: Value): string =
    failwith "not implemented"

let jstring (value: string): Value = JString value

let jint (value: int): Value = JNumber (float value)

let jfloat (value: float): Value = JNumber value

let jbool (value: bool): Value = JBool value

let jnull: Value = JNull

let jobject (props: (string * Value) seq) : Value =
    JObject (Map.ofSeq props)

let jlist (list: Value seq) : Value =
    JArray (List.ofSeq list)
