module FSharp.Json.Decode

open Chessie.ErrorHandling
open FSharp.Data

type Value = Encode.Value

type Decoder<'a> = Decoder of (Value -> Result<'a, string>)

let fail (msg: string): Decoder<_> =
    Decoder (fun _ -> fail msg)

let succeed (a: 'a) : Decoder<'a> =
    Decoder (fun _ -> ok a)

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

let private apply (Decoder decoder) v = decoder v

let private object' f =
    Decoder (function
        | JsonValue.Record _ as record -> f record
        | value -> crash "a Object" value)

let object1 (mapping: 'a -> 'value) decoder : Decoder<'value> =
    object' (apply decoder >> Trial.lift mapping)

let object2 (mapping: 'a -> 'b -> 'value) (decoder1: Decoder<'a>) (decoder2: Decoder<'b>) : Decoder<'value> =
    object' (fun o -> trial {
        let! prop1 = apply decoder1 o
        let! prop2 = apply decoder2 o
        return mapping prop1 prop2
    })

let object3 (mapping: 'a -> 'b -> 'c -> 'value) (decoder1 : Decoder<'a>) (decoder2: Decoder<'b>) (decoder3: Decoder<'c>) : Decoder<'value> =
    object' (fun o -> trial {
        let! prop1 = apply decoder1 o
        let! prop2 = apply decoder2 o
        let! prop3 = apply decoder3 o
        return mapping prop1 prop2 prop3
    })

let object4 mapping decoder1 decoder2 decoder3 decoder4 =
    object' (fun o -> trial {
        let! prop1 = apply decoder1 o
        let! prop2 = apply decoder2 o
        let! prop3 = apply decoder3 o
        let! prop4 = apply decoder4 o
        return mapping prop1 prop2 prop3 prop4
    })

let object5 mapping decoder1 decoder2 decoder3 decoder4 decoder5 =
    object' (fun o -> trial {
        let! prop1 = apply decoder1 o
        let! prop2 = apply decoder2 o
        let! prop3 = apply decoder3 o
        let! prop4 = apply decoder4 o
        let! prop5 = apply decoder5 o
        return mapping prop1 prop2 prop3 prop4 prop5
    })

let object6 mapping decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 =
    object' (fun o -> trial {
        let! prop1 = apply decoder1 o
        let! prop2 = apply decoder2 o
        let! prop3 = apply decoder3 o
        let! prop4 = apply decoder4 o
        let! prop5 = apply decoder5 o
        let! prop6 = apply decoder6 o
        return mapping prop1 prop2 prop3 prop4 prop5 prop6
    })

let object7 mapping decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 =
    object' (fun o -> trial {
        let! prop1 = apply decoder1 o
        let! prop2 = apply decoder2 o
        let! prop3 = apply decoder3 o
        let! prop4 = apply decoder4 o
        let! prop5 = apply decoder5 o
        let! prop6 = apply decoder6 o
        let! prop7 = apply decoder7 o
        return mapping prop1 prop2 prop3 prop4 prop5 prop6 prop7
    })

let object8 mapping decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 =
    object' (fun o -> trial {
        let! prop1 = apply decoder1 o
        let! prop2 = apply decoder2 o
        let! prop3 = apply decoder3 o
        let! prop4 = apply decoder4 o
        let! prop5 = apply decoder5 o
        let! prop6 = apply decoder6 o
        let! prop7 = apply decoder7 o
        let! prop8 = apply decoder8 o
        return mapping prop1 prop2 prop3 prop4 prop5 prop6 prop7 prop8
    })

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

let tuple' c f =
    Decoder (function
        | JsonValue.Array els when els.Length = c -> f els
        | value -> crash (sprintf "a Tuple of length %i" c) value)

let tuple1 (f: 'a -> 'value) (Decoder decoder: Decoder<'a>) : Decoder<'value> =
    tuple' 1 (fun arr -> decoder arr.[0] |> Trial.lift f)

let tuple2 (f: 'a -> 'b -> 'value) (Decoder decoder1) (Decoder decoder2) : Decoder<'value> =
    tuple' 2 (fun arr -> trial {
        let! res1 = decoder1 arr.[0]
        let! res2 = decoder2 arr.[1]
        return f res1 res2
    })

let tuple3 f (Decoder decoder1) (Decoder decoder2) (Decoder decoder3) =
    tuple' 3 (fun arr -> trial {
        let! res1 = decoder1 arr.[0]
        let! res2 = decoder2 arr.[1]
        let! res3 = decoder3 arr.[2]
        return f res1 res2 res3
    })

let tuple4 f (Decoder decoder1) (Decoder decoder2) (Decoder decoder3) (Decoder decoder4) =
    tuple' 4 (fun arr -> trial {
        let! res1 = decoder1 arr.[0]
        let! res2 = decoder2 arr.[1]
        let! res3 = decoder3 arr.[2]
        let! res4 = decoder4 arr.[3]
        return f res1 res2 res3 res4
    })

let tuple5 f (Decoder decoder1) (Decoder decoder2) (Decoder decoder3) (Decoder decoder4) (Decoder decoder5) =
    tuple' 5 (fun arr -> trial {
        let! res1 = decoder1 arr.[0]
        let! res2 = decoder2 arr.[1]
        let! res3 = decoder3 arr.[2]
        let! res4 = decoder4 arr.[3]
        let! res5 = decoder5 arr.[4]
        return f res1 res2 res3 res4 res5
    })

let tuple6 f (Decoder decoder1) (Decoder decoder2) (Decoder decoder3) (Decoder decoder4)
             (Decoder decoder5) (Decoder decoder6) =
    tuple' 6 (fun arr -> trial {
        let! res1 = decoder1 arr.[0]
        let! res2 = decoder2 arr.[1]
        let! res3 = decoder3 arr.[2]
        let! res4 = decoder4 arr.[3]
        let! res5 = decoder5 arr.[4]
        let! res6 = decoder6 arr.[5]
        return f res1 res2 res3 res4 res5 res6
    })

let tuple7 f (Decoder decoder1) (Decoder decoder2) (Decoder decoder3) (Decoder decoder4)
             (Decoder decoder5) (Decoder decoder6) (Decoder decoder7) =
    tuple' 7 (fun arr -> trial {
        let! res1 = decoder1 arr.[0]
        let! res2 = decoder2 arr.[1]
        let! res3 = decoder3 arr.[2]
        let! res4 = decoder4 arr.[3]
        let! res5 = decoder5 arr.[4]
        let! res6 = decoder6 arr.[5]
        let! res7 = decoder7 arr.[6]
        return f res1 res2 res3 res4 res5 res6 res7
    })

let tuple8 f (Decoder decoder1) (Decoder decoder2) (Decoder decoder3) (Decoder decoder4)
             (Decoder decoder5) (Decoder decoder6) (Decoder decoder7) (Decoder decoder8) =
    tuple' 8 (fun arr -> trial {
        let! res1 = decoder1 arr.[0]
        let! res2 = decoder2 arr.[1]
        let! res3 = decoder3 arr.[2]
        let! res4 = decoder4 arr.[3]
        let! res5 = decoder5 arr.[4]
        let! res6 = decoder6 arr.[5]
        let! res7 = decoder7 arr.[6]
        let! res8 = decoder8 arr.[7]
        return f res1 res2 res3 res4 res5 res6 res7 res8
    })

let customDecoder (Decoder decoder: Decoder<'a>) (callback: 'a -> Result<'b, _>) : Decoder<'b> =
    Decoder (fun value ->
        match decoder value with
        | Ok (v, _) -> callback v
        | Bad err -> Bad err)
