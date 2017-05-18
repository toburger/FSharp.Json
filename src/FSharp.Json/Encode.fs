module FSharp.Json.Encode

let escapeString (value: string) =
    let stringBuilder = System.Text.StringBuilder(value.Length)
    ignore (stringBuilder.Append "\"")
    for char in value do
        let newChars =
            match char with
            | '"'    -> "\\\""
            | '\\'   -> "\\\\"
            | '/'    -> "\\/"
            //| '\NUL' -> "\\u0000"
            //| '\SOH' -> "\\u0001"
            //| '\STX' -> "\\u0002"
            //| '\ETX' -> "\\u0003"
            //| '\EOT' -> "\\u0004"
            //| '\ENQ' -> "\\u0005"
            //| '\ACK' -> "\\u0006"
            | '\a'   -> "\\u0007"
            | '\b'   -> "\\b"
            | '\t'   -> "\\t"
            | '\n'   -> "\\n"
            | '\v'   -> "\\u000b"
            | '\f'   -> "\\f"
            | '\r'   -> "\\r"
            //| '\SO'  -> "\\u000e"
            //| '\SI'  -> "\\u000f"
            //| '\DLE' -> "\\u0010"
            //| '\DC1' -> "\\u0011"
            //| '\DC2' -> "\\u0012"
            //| '\DC3' -> "\\u0013"
            //| '\DC4' -> "\\u0014"
            //| '\NAK' -> "\\u0015"
            //| '\SYN' -> "\\u0016"
            //| '\ETB' -> "\\u0017"
            //| '\CAN' -> "\\u0018"
            //| '\EM'  -> "\\u0019"
            //| '\SUB' -> "\\u001a"
            //| '\ESC' -> "\\u001b"
            //| '\FS'  -> "\\u001c"
            //| '\GS'  -> "\\u001d"
            //| '\RS'  -> "\\u001e"
            //| '\US'  -> "\\u001f"
            //| '\DEL' -> "\\u007f"
            | c      -> string c
        ignore (stringBuilder.Append newChars)
    ignore (stringBuilder.Append "\"")
    stringBuilder.ToString()

let NULL = "null"
let TRUE = "true"
let FALSE = "false"

let rec format = function
  | JString null -> NULL
  | JString s -> escapeString s
  | JNumber i -> string i
  | JBool true -> TRUE
  | JBool false -> FALSE
  | JNull -> NULL
  | JObject list ->
      list
      |> List.map (fun (k, v) ->
        sprintf "%s:%s" (escapeString k) (format v))
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

let jint (value: int): JValue = JNumber (decimal value)

let jfloat (value: float): JValue =
    if System.Double.IsNaN value
    then JNull
    else JNumber (decimal value)

let jdecimal (value: decimal): JValue =
    JNumber value

let jbool (value: bool): JValue = JBool value

let jnull: JValue = JNull

let jobject (props: (string * JValue) seq) : JValue =
    props
    |> Seq.toList
    |> JObject

let jlist (list: JValue seq) : JValue =
    JArray (Seq.toList list)
