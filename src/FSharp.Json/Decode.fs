module FSharp.Json.Decode

open Newtonsoft.Json

type Value = Encode.Value

type Decoder<'a> = Decoder of (Value -> Result<string, 'a>)

let fail (msg: string): Decoder<_> =
    Decoder (fun _ -> Err msg)

let succeed (a: 'a) : Decoder<'a> =
    Decoder (fun _ -> Ok a)

let private crash expected actual =
    Err (sprintf "expecting %s but got %s" expected (serialize actual))

let bind (binder: 'a -> Decoder<'b>) (Decoder decoder): Decoder<'b> =
    Decoder (fun value ->
        match decoder value with
        | Ok v ->
            let (Decoder result) = binder v
            result value
        | Err e -> Err e)

let (>>=) decoder binder = bind binder decoder

let map (mapper: 'a -> 'b) (Decoder decoder): Decoder<'b> =
    Decoder (decoder >> Result.map mapper)

let decodeString (Decoder decoder) (s : string) : Result<string, 'a> =
    decoder (parse s)

let decodeField (field: string) (Decoder decoder) : Decoder<'b> =
    Decoder (function
        | :? Linq.JProperty as p when p.Name = field ->
            decoder p.Value
        | value -> crash "a Property" value)

let (:=) = decodeField

let private getProperty (Decoder decoder) (o: Linq.JObject) =
    o.Properties()
    |> Seq.tryPick (fun p ->
        match decoder p with
        | Ok v -> Some (Ok v)
        | _ -> None)
    |> function
        | Some v -> v
        | None -> crash "a Property" o

let private object' f =
    Decoder (function
        | :? Linq.JObject as o -> f o
        | value -> crash "a Object" value)

let object1 (mapping: 'a -> 'value) decoder : Decoder<'value> =
    object' (getProperty decoder >> Result.map mapping)

let object2 (mapping: 'a -> 'b -> 'value) (decoder1: Decoder<'a>) (decoder2: Decoder<'b>) : Decoder<'value> =
    object' (fun o -> result {
        let! prop1 = getProperty decoder1 o
        let! prop2 = getProperty decoder2 o
        return mapping prop1 prop2
    })

let object3 (mapping: 'a -> 'b -> 'c -> 'value) (decoder1 : Decoder<'a>) (decoder2: Decoder<'b>) (decoder3: Decoder<'c>) : Decoder<'value> =
    object' (fun o -> result {
        let! prop1 = getProperty decoder1 o
        let! prop2 = getProperty decoder2 o
        let! prop3 = getProperty decoder3 o
        return mapping prop1 prop2 prop3
    })

let object4 mapping decoder1 decoder2 decoder3 decoder4 =
    object' (fun o -> result {
        let! prop1 = getProperty decoder1 o
        let! prop2 = getProperty decoder2 o
        let! prop3 = getProperty decoder3 o
        let! prop4 = getProperty decoder4 o
        return mapping prop1 prop2 prop3 prop4
    })

let object5 mapping decoder1 decoder2 decoder3 decoder4 decoder5 =
    object' (fun o -> result {
        let! prop1 = getProperty decoder1 o
        let! prop2 = getProperty decoder2 o
        let! prop3 = getProperty decoder3 o
        let! prop4 = getProperty decoder4 o
        let! prop5 = getProperty decoder5 o
        return mapping prop1 prop2 prop3 prop4 prop5
    })

let object6 mapping decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 =
    object' (fun o -> result {
        let! prop1 = getProperty decoder1 o
        let! prop2 = getProperty decoder2 o
        let! prop3 = getProperty decoder3 o
        let! prop4 = getProperty decoder4 o
        let! prop5 = getProperty decoder5 o
        let! prop6 = getProperty decoder6 o
        return mapping prop1 prop2 prop3 prop4 prop5 prop6
    })

let object7 mapping decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 =
    object' (fun o -> result {
        let! prop1 = getProperty decoder1 o
        let! prop2 = getProperty decoder2 o
        let! prop3 = getProperty decoder3 o
        let! prop4 = getProperty decoder4 o
        let! prop5 = getProperty decoder5 o
        let! prop6 = getProperty decoder6 o
        let! prop7 = getProperty decoder7 o
        return mapping prop1 prop2 prop3 prop4 prop5 prop6 prop7
    })

let object8 mapping decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 =
    object' (fun o -> result {
        let! prop1 = getProperty decoder1 o
        let! prop2 = getProperty decoder2 o
        let! prop3 = getProperty decoder3 o
        let! prop4 = getProperty decoder4 o
        let! prop5 = getProperty decoder5 o
        let! prop6 = getProperty decoder6 o
        let! prop7 = getProperty decoder7 o
        let! prop8 = getProperty decoder8 o
        return mapping prop1 prop2 prop3 prop4 prop5 prop6 prop7 prop8
    })

let dvalue (decoder: obj -> Result<_, 'a>) : Decoder<'a> =
    Decoder (function
        | :? Linq.JValue as v -> decoder v.Value
        | value -> crash "a Value" value)

let dbool : Decoder<bool> =
    dvalue (function
        | :? bool as b -> Ok b
        | value -> crash "a Boolean" value)

let dstring : Decoder<string> =
    dvalue (function
        | :? string as s -> Ok s
        | value -> crash "a String" value)

let decodeWith<'a> : Decoder<'a> =
    dvalue (function
        | :? 'a as a -> Ok a
        | value -> crash (sprintf "a %s" typeof<'a>.FullName) value)

let dfloat : Decoder<float> =
    dvalue (function
        | :? float as s -> Ok s
        | value -> crash "a Float" value)

let dint : Decoder<int> =
    dvalue (function
        | :? int as i -> Ok i
        | :? int64 as i -> Ok (int i)
        | value -> crash "a Int" value)

let list (Decoder decoder : Decoder<'a>) : Decoder<list<'a>> =
    Decoder (function
        | :? Linq.JArray as arr ->
            arr.Values()
            |> Seq.map decoder
            |> Seq.toList
            |> Result.transform
            |> Result.mapError List.head
        |  value -> crash "a Array" value)

let dnull (v: 'a) : Decoder<'a> =
    Decoder (function
        | null -> Ok v
        | value -> crash "null" value)

let maybe (Decoder decoder: Decoder<'a>) : Decoder<option<'a>> =
    Decoder (function
        | null -> Ok None
        | v ->
            decoder v
            |> Result.bind (Ok << Some))

let oneOf (decoders: list<Decoder<'a>>) : Decoder<'a> =
    Decoder (fun v ->
        List.fold (fun s (Decoder decoder) ->
            match decoder v, s with
            | Ok v, _-> Ok v
            | Err _, Ok ok -> Ok ok
            | Err err, Err errs -> Err (err::errs))
            (Err [])
            decoders
        |> Result.mapError (sprintf "expecting one of the following: %A"))

let keyValuePairs (Decoder decoder: Decoder<'a>) : Decoder<list<string * 'a>> =
    Decoder (function
        | :? Linq.JObject as o ->
            o.Properties()
            |> Seq.map (fun prop ->
                decoder prop.Value
                |> Result.map (fun v -> prop.Name, v))
            |> Seq.toList
            |> Result.transform
            |> Result.mapError List.head
        | value -> crash "an Object" value)

let dmap (decoder: Decoder<'a>) : Decoder<Map<string, 'a>> =
    map Map.ofList (keyValuePairs decoder)

let dobject (decoder: Decoder<'a>) : Decoder<'a> =
    Decoder (function
        | :? Linq.JObject as v -> getProperty decoder v
        | value -> crash "a Value" value)

let at (fields: list<string>) (decoder: Decoder<'a>) : Decoder<'a> =
    List.foldBack (fun field d -> dobject (field := d)) fields decoder

let value : Decoder<Value> =
    Decoder Ok

let decodeValue (Decoder decoder: Decoder<'a>) (value: Value) : Result<string, 'a> =
    decoder value

let tuple1 (f: 'a -> 'value) (Decoder decoder: Decoder<'a>) : Decoder<'value> =
    Decoder (function
        | :? Linq.JArray as arr when arr.Count = 1 ->
            decoder arr.First |> Result.map f
        | value -> crash "a Tuple of length 1" value)

let tuple2 (f: 'a -> 'b -> 'value) (Decoder decoder1) (Decoder decoder2) : Decoder<'value> =
    Decoder (function
        | :? Linq.JArray as arr when arr.Count = 2 ->
            result {
                let! res1 = decoder1 <| arr.Item(0)
                let! res2 = decoder2 <| arr.Item(1)
                return f res1 res2
            }
        | value -> crash "a Tuple of length 2" value)

let tuple3 f (Decoder decoder1) (Decoder decoder2) (Decoder decoder3) =
    Decoder (function
        | :? Linq.JArray as arr when arr.Count = 3 ->
            result {
                let! res1 = decoder1 <| arr.Item(0)
                let! res2 = decoder2 <| arr.Item(1)
                let! res3 = decoder3 <| arr.Item(2)
                return f res1 res2 res3
            }
        | value -> crash "a Tuple of length 3" value)

let tuple4 f (Decoder decoder1) (Decoder decoder2) (Decoder decoder3) (Decoder decoder4) =
    Decoder (function
        | :? Linq.JArray as arr when arr.Count = 4 ->
            result {
                let! res1 = decoder1 <| arr.Item(0)
                let! res2 = decoder2 <| arr.Item(1)
                let! res3 = decoder3 <| arr.Item(2)
                let! res4 = decoder4 <| arr.Item(3)
                return f res1 res2 res3 res4
            }
        | value -> crash "a Tuple of length 4" value)

let customDecoder (Decoder decoder: Decoder<'a>) (callback: 'a -> Result<string, 'b>) : Decoder<'b> =
    Decoder (fun value ->
        match decoder value with
        | Ok v -> callback v
        | Err err -> Err err)
