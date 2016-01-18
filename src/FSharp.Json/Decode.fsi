module FSharp.Json.Decode

open Chessie.ErrorHandling

/// Represents a JavaScript value.
type Value = FSharp.Json.Utils.Value

/// Represents a way of decoding JSON values. If you have a (Decoder (List String)) it will attempt to take some JSON value and turn it into a list of strings. These decoders are easy to put together so you can create more and more complex decoders.
type Decoder<'a> = Decoder of (Value -> Result<'a, string>)

/// A decoder that always succeeds. Useful when paired with andThen or oneOf but everything is supposed to work out at the end. For example, maybe you have an optional field that can have a default value when it is missing.
val succeed : 'a -> Decoder<'a>

/// A decoder that always fails. Useful when paired with andThen or oneOf to improve error messages when things go wrong. For example, the following decoder is able to provide a much more specific error message when fail is the last option.
val fail : string -> Decoder<'a>

/// Transform the value returned by a decoder. Most useful when paired with the oneOf function.
val map : ('a -> 'b) -> Decoder<'a> -> Decoder<'b>

/// Helpful when one field will determine the shape of a bunch of other fields.
val bind : ('a -> Decoder<'b>) -> Decoder<'a> -> Decoder<'b>

/// Using a certain decoder, attempt to parse a raw Json.Value. You can pass a Json.Value into Elm through a port, so this can let you handle data with extra weird shapes or stuff that currently is not allowed through ports automatically.
val decodeValue : Decoder<'a> -> Value -> Result<'a, string>

/// Using a certain decoder, attempt to parse a JSON string. If the decoder fails, you will get a string message telling you why.
val decodeString: Decoder<'a> -> string -> Result<'a, string>

/// Extract a string.
val dstring : Decoder<string>

/// Extract an integer.
val dint : Decoder<int>

/// Extract a float.
val dfloat : Decoder<float>

/// Extract a boolean.
val dbool : Decoder<bool>

/// Decode null as the value given, and fail otherwise. Primarily useful for creating other decoders.
val dnull : 'a -> Decoder<'a>

/// Extract a List from a JS array.
val dlist : Decoder<'a> -> Decoder<'a list>

/// Extract a Maybe value, wrapping successes with Just and turning any failure in Nothing. If you are expecting that a field can sometimes be null, it's better to check for it explicitly, as this function will swallow errors from ill-formed JSON.
val maybe : Decoder<'a> -> Decoder<'a option>

/// Try out multiple different decoders. This is helpful when you are dealing with something with a very strange shape and when andThen does not help narrow things down so you can be more targeted.
val oneOf : Decoder<'a> list -> Decoder<'a>

/// Applies the decoder to the field with the given name. Fails if the JSON object has no such field.
val (:=) : (string -> Decoder<'a> -> Decoder<'a>)

/// Apply a function to a decoder. You can use this function as map if you must (which can be done with any objectN function actually).
val object1 : ('a -> 'value) -> Decoder<'a> -> Decoder<'value>

/// Use two different decoders on a JS value. This is nice for extracting multiple fields from an object.
val object2 : ('a -> 'b -> 'value) -> Decoder<'a> -> Decoder<'b> -> Decoder<'value>

/// Use three different decoders on a JS value. This is nice for extracting multiple fields from an object.
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

/// Turn any object into a list of key-value pairs. Fails if any key can't be decoded with the given decoder.
val keyValuePairs : Decoder<'a> -> Decoder<(string * 'a) list>

/// Turn any object into a dictionary of key-value pairs.
val dmap : Decoder<'a> -> Decoder<Map<string, 'a>>

/// Access a nested field, making it easy to dive into big structures. This is really a helper function so you do not need to write (:=) so many times.
val at : string list -> Decoder<'a> -> Decoder<'a>

/// Handle an array with exactly one element.
val tuple1 : f: ('a -> 'value) -> Decoder<'a> -> Decoder<'value>

/// Handle an array with exactly two elements. Useful for points and simple pairs.
val tuple2 : ('a -> 'b -> 'value) -> Decoder<'a> -> Decoder<'b> -> Decoder<'value>

/// Handle an array with exactly three elements.
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

/// Bring in an arbitrary JSON value. Useful if you need to work with crazily formatted data. For example, this lets you create a parser for "variadic" lists where the first few types are different, followed by 0 or more of the same type.
val value : Decoder<Value>

/// Create a custom decoder that may do some fancy computation. See the value documentation for an example usage.
val customDecoder : Decoder<'a> -> callback: ('a -> Result<'value, string>) -> Decoder<'value>
