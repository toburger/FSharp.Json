module FSharp.Json.Encode

type Value = FSharp.Json.Utils.Value

val encode : indent: bool -> value: Value -> string

val jstring : value: string -> Value

val jint : value: int -> Value

val jfloat : value: float -> Value

val jbool : value: bool -> Value

val jnull : Value

val jobject : props: (string * Value) seq -> Value

val jlist : list: Value seq -> Value
