module FSharp.Json.Decode

open Chessie.ErrorHandling
open FSharp.Data

type Value = Encode.Value

type Decoder<'a> = Decoder of (Value -> Result<'a, string>)

let fail (msg: string): Decoder<_> =
    Decoder (fun _ -> fail msg)

let succeed (a: 'a) : Decoder<'a> =
    Decoder (fun _ -> ok a)

let private run (Decoder decoder) v = decoder v

let private crash expected actual =
    Trial.fail (sprintf "expecting %s but got %s" expected (serialize actual))

let bind (binder: 'a -> Decoder<'b>) (Decoder decoder): Decoder<'b> =
    Decoder (fun value ->
        match decoder value with
        | Ok (v, _) ->
            let (Decoder result) = binder v
            result value
        | Bad e -> Bad e)

let (>>=) decoder binder = bind binder decoder

let map (mapper: 'a -> 'b) (Decoder decoder): Decoder<'b> =
    Decoder (decoder >> Trial.lift mapper)

let (<!>) = map

let apply (Decoder f: Decoder<'a -> 'b>) (Decoder d: Decoder<'a>): Decoder<'b> =
    Decoder (fun value ->
        trial {
            let! f = f value
            let! a = d value
            return f a
        })

let (<*>) = apply

let decodeString (Decoder decoder) (s : string) : Result<_, _> =
    decoder (parse s)

let (|RecordField|_|) field record =
    match record with
    | JsonValue.Record properties ->
        properties
        |> Array.tryPick (fun (n, v) ->
            if n = field
            then Some v
            else None)
    | _ -> None

let decodeField (field: string) (Decoder decoder) : Decoder<'b> =
    Decoder (function
        | RecordField field value ->
            decoder value
        | value -> crash "a Property" value)

let (:=) = decodeField

let dobject =
    Decoder (function | JsonValue.Record v -> ok v | v -> crash "a Object" v)

let object1 mapping decoder =
    dobject >>= fun _ -> succeed mapping <*> decoder

let object2 mapping decoder1 decoder2 =
    dobject >>= (fun _ -> mapping <!> decoder1 <*> decoder2)

let object3 mapping d1 d2 d3 =
    dobject >>= (fun _ -> mapping <!> d1 <*> d2 <*> d3)

let object4 mapping d1 d2 d3 d4 =
    dobject >>= (fun _ -> mapping <!> d1 <*> d2 <*> d3 <*> d4)

let object5 mapping d1 d2 d3 d4 d5 =
    dobject >>= (fun _ -> mapping <!> d1 <*> d2 <*> d3 <*> d4 <*> d5)

let object6 mapping d1 d2 d3 d4 d5 d6 =
    dobject >>= (fun _ -> mapping <!> d1 <*> d2 <*> d3 <*> d4 <*> d5 <*> d6)

let object7 mapping d1 d2 d3 d4 d5 d6 d7 =
    dobject >>= (fun _ -> mapping <!> d1 <*> d2 <*> d3 <*> d4 <*> d5 <*> d6 <*> d7)

let object8 mapping d1 d2 d3 d4 d5 d6 d7 d8 =
    dobject >>= (fun _ -> mapping <!> d1 <*> d2 <*> d3 <*> d4 <*> d5 <*> d6 <*> d7 <*> d8)

let dvalue (decoder: Value -> Result<'a, _>) : Decoder<'a> =
    Decoder decoder

let dbool : Decoder<bool> =
    dvalue (function
        | JsonValue.Boolean b -> ok b
        | value -> crash "a Boolean" value)

let dstring : Decoder<string> =
    dvalue (function
        | JsonValue.String s -> ok s
        | value -> crash "a String" value)

let dfloat : Decoder<float> =
    dvalue (function
        | JsonValue.Number n -> ok (float n)
        | value -> crash "a Float" value)

let dint : Decoder<int> =
    dvalue (function
        | JsonValue.Number n -> ok (int n)
        | value -> crash "a Int" value)

let dlist (Decoder decoder : Decoder<'a>) : Decoder<list<'a>> =
    Decoder (function
        | JsonValue.Array elems ->
            elems
            |> Seq.map decoder
            |> Trial.collect
        |  value -> crash "a Array" value)

let dnull (v: 'a) : Decoder<'a> =
    Decoder (function
        | JsonValue.Null -> ok v
        | value -> crash "null" value)

let maybe (Decoder decoder: Decoder<'a>) : Decoder<option<'a>> =
    Decoder (decoder >> Trial.either (fun (v, _) -> ok (Some v))
                                     (fun _ -> ok None))

let oneOf (decoders: list<Decoder<'a>>) : Decoder<'a> =
    Decoder (fun v ->
        List.fold (fun s (Decoder decoder) ->
            match decoder v, s with
            | Ok (v, w), _-> Ok (v, w)
            | Bad _, Ok (ok, w) -> Ok (ok, w)
            | Bad err, Bad errs -> Bad (err @ errs))
            (Bad [])
            decoders)

let keyValuePairs (Decoder decoder: Decoder<'a>) : Decoder<list<string * 'a>> =
    Decoder (function
        | JsonValue.Record props ->
            props
            |> Seq.map (fun (name, value) ->
                decoder value
                |> Trial.lift (fun v -> name, v))
            |> Trial.collect
        | value -> crash "an Object" value)

let dmap (decoder: Decoder<'a>) : Decoder<Map<string, 'a>> =
    map Map.ofList (keyValuePairs decoder)

let at (fields: list<string>) (decoder: Decoder<'a>) : Decoder<'a> =
    List.foldBack (:=) fields decoder

let value : Decoder<Value> =
    Decoder ok

let decodeValue (Decoder decoder: Decoder<'a>) (value: Value) : Result<'a, _> =
    decoder value

let dtuple c =
    Decoder (function
        | JsonValue.Array els when els.Length = c -> ok els
        | value -> crash (sprintf "a Tuple of length %i" c) value)

let always d v =
    Decoder (fun _ -> run d v)

let tuple1 (f: 'a -> 'b) (d: Decoder<'a>) : Decoder<'b> =
    dtuple 1 >>= fun arr ->
        succeed f <*> always d arr.[0]

let tuple2 f d1 d2 =
    dtuple 2 >>= fun arr ->
        f <!> always d1 arr.[0]
          <*> always d2 arr.[1]

let tuple3 f d1 d2 d3 =
    dtuple 3 >>= fun arr ->
        f <!> always d1 arr.[0]
          <*> always d2 arr.[1]
          <*> always d3 arr.[2]

let tuple4 f d1 d2 d3 d4 =
    dtuple 4 >>= fun arr ->
        f <!> always d1 arr.[0]
          <*> always d2 arr.[1]
          <*> always d3 arr.[2]
          <*> always d4 arr.[3]

let tuple5 f d1 d2 d3 d4 d5 =
    dtuple 5 >>= fun arr ->
        f <!> always d1 arr.[0]
          <*> always d2 arr.[1]
          <*> always d3 arr.[2]
          <*> always d4 arr.[3]
          <*> always d5 arr.[4]

let tuple6 f d1 d2 d3 d4 d5 d6 =
    dtuple 6 >>= fun arr ->
        f <!> always d1 arr.[0]
          <*> always d2 arr.[1]
          <*> always d3 arr.[2]
          <*> always d4 arr.[3]
          <*> always d5 arr.[4]
          <*> always d6 arr.[5]

let tuple7 f d1 d2 d3 d4 d5 d6 d7 =
    dtuple 7 >>= fun arr ->
        f <!> always d1 arr.[0]
          <*> always d2 arr.[1]
          <*> always d3 arr.[2]
          <*> always d4 arr.[3]
          <*> always d5 arr.[4]
          <*> always d6 arr.[5]
          <*> always d7 arr.[6]
          
let tuple8 f d1 d2 d3 d4 d5 d6 d7 d8 =
    dtuple 8 >>= fun arr ->
        f <!> always d1 arr.[0]
          <*> always d2 arr.[1]
          <*> always d3 arr.[2]
          <*> always d4 arr.[3]
          <*> always d5 arr.[4]
          <*> always d6 arr.[5]
          <*> always d7 arr.[6]
          <*> always d8 arr.[7]

let customDecoder (Decoder decoder: Decoder<'a>) (callback: 'a -> Result<'b, _>) : Decoder<'b> =
    Decoder (fun value ->
        match decoder value with
        | Ok (v, _) -> callback v
        | Bad err -> Bad err)
