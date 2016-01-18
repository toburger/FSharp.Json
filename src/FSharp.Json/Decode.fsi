module FSharp.Json.Decode

open Chessie.ErrorHandling

type Value = FSharp.Json.Utils.Value
type Decoder<'a> = Decoder of (Value -> Result<'a, string>)

val succeed : 'a -> Decoder<'a>

val fail : string -> Decoder<'a>

val map : ('a -> 'b) -> Decoder<'a> -> Decoder<'b>

val bind : ('a -> Decoder<'b>) -> Decoder<'a> -> Decoder<'b>

val decodeValue : Decoder<'a> -> Value -> Result<'a, string>

val decodeString: Decoder<'a> -> string -> Result<'a, string>

val dstring : Decoder<string>

val dint : Decoder<int>

val dfloat : Decoder<float>

val dbool : Decoder<bool>

val dnull : 'a -> Decoder<'a>

val maybe : Decoder<'a> -> Decoder<'a option>

val oneOf : Decoder<'a> list -> Decoder<'a>

val dlist : Decoder<'a> -> Decoder<'a list>

val (:=) : (string -> Decoder<'a> -> Decoder<'a>)

val object1 : ('a -> 'value) -> Decoder<'a> -> Decoder<'value>

val object2 : ('a -> 'b -> 'value) -> Decoder<'a> -> Decoder<'b> -> Decoder<'value>

val object3 : ('a -> 'b -> 'c -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'value>

val object4 : ('a -> 'b -> 'c -> 'd -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'d> -> Decoder<'value>

val object5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'d> -> Decoder<'e> -> Decoder<'value>

val object6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'d> -> Decoder<'e> -> Decoder<'f> -> Decoder<'value>

val object7 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'d> -> Decoder<'e> -> Decoder<'f> ->
              Decoder<'g> -> Decoder<'value>

val object8 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value) -> Decoder<'a> ->
              Decoder<'b> -> Decoder<'c> -> Decoder<'d> -> Decoder<'e> -> Decoder<'f> ->
              Decoder<'g> -> Decoder<'h> -> Decoder<'value>

val keyValuePairs : Decoder<'a> -> Decoder<(string * 'a) list>

val dmap : Decoder<'a> -> Decoder<Map<string, 'a>>

val at : string list -> Decoder<'a> -> Decoder<'a>

val tuple1 : f: ('a -> 'value) -> Decoder<'a> -> Decoder<'value>

val tuple2 : ('a -> 'b -> 'value) -> Decoder<'a> -> Decoder<'b> -> Decoder<'value>

val tuple3 : ('a -> 'b -> 'c -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'value>

val tuple4 : ('a -> 'b -> 'c -> 'd -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'d> -> Decoder<'value>

val tuple5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'d> -> Decoder<'e> -> Decoder<'value>

val tuple6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'d> -> Decoder<'e> -> Decoder<'f> -> Decoder<'value>

val tuple7 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value) -> Decoder<'a> -> Decoder<'b> ->
              Decoder<'c> -> Decoder<'d> -> Decoder<'e> -> Decoder<'f> ->
              Decoder<'g> -> Decoder<'value>

val tuple8 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value) -> Decoder<'a> ->
              Decoder<'b> -> Decoder<'c> -> Decoder<'d> -> Decoder<'e> -> Decoder<'f> ->
              Decoder<'g> -> Decoder<'h> -> Decoder<'value>

val value : Decoder<Value>

val customDecoder : Decoder<'a> -> callback: ('a -> Result<'value, string>) -> Decoder<'value>
