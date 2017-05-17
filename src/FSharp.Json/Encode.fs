module FSharp.Json.Encode

let rec format = function
  | JString s -> sprintf "\"%s\"" s
  | JNumber i -> string i
  | JBool true -> "true"
  | JBool false -> "false"
  | JNull -> "null"
  | JObject list ->
      list
      |> List.map (fun (k, v) ->
        sprintf "\"%s\":%s" k (format v))
      |> String.concat ","
      |> sprintf "{%s}"
  | JArray list ->
      list
      |> List.map format
      |> String.concat ","
      |> sprintf "[%s]"

let encode indent value =
    if indent then failwith "parser cannot produce indented JSON at the moment"
    format value

let jstring (value: string): JValue = JString value

let jint (value: int): JValue = JNumber (float value)

let jfloat (value: float): JValue = JNumber (float value)

let jbool (value: bool): JValue = JBool value

let jnull: JValue = JNull

let jobject (props: (string * JValue) seq) : JValue =
    props
    |> Seq.toList
    |> JObject

let jlist (list: JValue seq) : JValue =
    JArray (Seq.toList list)
