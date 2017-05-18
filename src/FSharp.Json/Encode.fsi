/// Library for turning F# values into Json values.
module FSharp.Json.Encode

/// Convert a Value into a prettified string. The first argument specifies the indentation in the resulting string.
val encode : indent: bool -> value: JValue -> string

val jstring : value: string -> JValue

val jint : value: int -> JValue

val jfloat : value: float -> JValue

val jdecimal : value: decimal -> JValue

val jbool : value: bool -> JValue

val jnull : JValue

val jobject : props: (string * JValue) seq -> JValue

val jlist : list: JValue seq -> JValue
