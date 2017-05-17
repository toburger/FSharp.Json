namespace FSharp.Json

type JValue =
    | JString of string
    | JNumber of float
    | JBool   of bool
    | JNull
    | JObject of (string * JValue) list
    | JArray  of JValue list
