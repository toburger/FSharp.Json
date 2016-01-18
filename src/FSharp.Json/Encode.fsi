module FSharp.Json.Encode

/// Represents a JavaScript value.
type Value = FSharp.Json.Utils.Value

/// Convert a Value into a prettified string. The first argument specifies the amount of indentation in the resulting string.
val encode : indent: bool -> value: Value -> string

val jstring : value: string -> Value

val jint : value: int -> Value

val jfloat : value: float -> Value

val jbool : value: bool -> Value

val jnull : Value

val jobject : props: (string * Value) seq -> Value

val jlist : list: Value seq -> Value
