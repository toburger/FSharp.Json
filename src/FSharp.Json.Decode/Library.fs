namespace FSharp.Json

open Newtonsoft.Json

[<AutoOpen>]
module private Helpers =
    let parse s = JsonConvert.DeserializeObject<Newtonsoft.Json.Linq.JToken> s
    let serialize (o: obj) = JsonConvert.SerializeObject o

module Encode =

    type Value = Newtonsoft.Json.Linq.JToken

    let encode indentLevel value =
        use sw = new System.IO.StringWriter()
        use writer = new JsonTextWriter(sw, Indentation = indentLevel)
        let serializer = JsonSerializer()
        serializer.Serialize(writer, value)
        sw.ToString()

//        let identity (value: obj): Value = value

module Decode =

    type Value = Encode.Value

    type Decoder<'a> = Decoder of (Value -> Result<string, 'a>)

    let fail (msg: string): Decoder<_> =
        Decoder (fun _ -> Err msg)

    let succeed (a: 'a) : Decoder<'a> =
        Decoder (fun _ -> Ok a)

    let private crash expected actual =
//            failwithf "expecting %s but got %s" expected (serialize actual)
        Err (sprintf "expecting %s but got %s" expected (serialize actual))

    let bind (binder: 'a -> Decoder<'b>) (Decoder decoder): Decoder<'b> =
        Decoder (fun value ->
            match decoder value with
            | Ok v ->
                let (Decoder result) = binder v
                result value
            | Err  e -> Err e)

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
//                downcast (value.GetType().GetProperty(field).GetValue(value)))

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

    let object1 (mapping: 'a -> 'value) decoder : Decoder<'value> =
        Decoder (function
            | :? Linq.JObject as o ->
                o
                |> getProperty decoder
                |> Result.map mapping
            | value -> crash "a Object" value)

    let object2 (mapping: 'a -> 'b -> 'value) (decoder1: Decoder<'a>) (decoder2: Decoder<'b>) : Decoder<'value> =
        Decoder (function
            | :? Linq.JObject as o ->
                result {
                    let! prop1 = getProperty decoder1 o
                    let! prop2 = getProperty decoder2 o
                    return mapping prop1 prop2
                }
            | value -> crash "a Object" value)

    let object3 (mapping: 'a -> 'b -> 'c -> 'value) (decoder1 : Decoder<'a>) (decoder2: Decoder<'b>) (decoder3: Decoder<'c>) : Decoder<'value> =
        Decoder (function
            | :? Linq.JObject as o ->
                result {
                    let! prop1 = getProperty decoder1 o
                    let! prop2 = getProperty decoder2 o
                    let! prop3 = getProperty decoder3 o
                    return mapping prop1 prop2 prop3
                }
            | value -> crash "a Object" value)

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
        Decoder (fun value ->
            match value with
            | :? Linq.JObject as o ->
                o.Properties()
                |> Seq.map (fun prop ->
                    decoder prop.Value
                    |> Result.map (fun v -> prop.Name, v))
                |> Seq.toList
                |> Result.transform
                |> Result.mapError List.head
            | _ -> crash "an Object" value)

    let dmap (decoder: Decoder<'a>) : Decoder<Map<string, 'a>> =
        map Map.ofList (keyValuePairs decoder)

    let dobject (decoder: Decoder<'a>) : Decoder<'a> =
        Decoder (function
            | :? Linq.JObject as v -> getProperty decoder v
            | value -> crash "a Value" value)

    let at (fields: list<string>) (decoder: Decoder<'a>) : Decoder<'a> =
        List.foldBack (fun field d -> dobject (field := d)) fields decoder

//    let value : Decoder<Value> =
//        failwith "not implemented"
//
//    let decodeValue (Decoder decoder: Decoder<'a>) (value: Value) : Result<string, 'a> =
//        failwith "not implemented"

    let customDecoder (Decoder decoder: Decoder<'a>) (callback: 'a -> Result<string, 'b>) : Decoder<'b> =
        Decoder (fun value ->
            match decoder value with
            | Ok v -> callback v
            | Err err -> Err err)
