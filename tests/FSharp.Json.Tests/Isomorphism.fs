module Isomorphism

open FsCheck
open FsCheck.NUnit
open FSharp.Json.Encode
open FSharp.Json.Decode

type MapKey = Key of string

type ListObject = {
    Int: int
    Float: float
    Decimal: decimal
    Tuple: (int * string)
}

type MapObject = {
    List: ListObject list
}

type Record = {
    Name: string
    Number: int
    MaybeNumber: int option
    List: ListObject list
    Map: Map<MapKey, MapObject>
}

type Union =
    | Single
    | Record of Record
    | Complex of int * ListObject list
    | Recursive of Union

module Encoders =
    let encodeListObject (o: ListObject) =
        jobject
            [ "int", jint o.Int
              "float", jfloat o.Float
              "decimal", jdecimal o.Decimal
              "tuple",
                jlist
                    [ jint (fst o.Tuple)
                      jstring (snd o.Tuple) ] ]

    let encodeList =
        jlist << List.map encodeListObject

    let encodeMapObject (o: MapObject) =
        jobject [ "list", encodeList o.List ]

    let encodeMap =
        Seq.map (fun (KeyValue(Key k, v)) ->
                    k, encodeMapObject v)
        >> jobject

    let encodeObject o =
        [ "name",  if o.Name = null then jnull else jstring o.Name
          "number", jint o.Number
          "maybeNumber", Option.fold (fun _ -> jint) jnull o.MaybeNumber
          "list", encodeList o.List
          "map", encodeMap o.Map ]

    let rec encodeUnion = function
        | Single ->
            jobject [ "type", jstring "single" ]
        | Record o ->
            jobject
                (("type", jstring "record")
                 :: encodeObject o)
        | Complex (n, xs) ->
            jobject
                [ "type", jstring "complex"
                  "number", jint n
                  "list", encodeList xs ]
        | Recursive u ->
            jobject
                [ "type", jstring "recursive"
                  "data", encodeUnion u ]

#nowarn "40"

module Decoders =
    let decodeListObject =
        object4
            (fun i f d t ->
                { Int = i
                  Float = f
                  Decimal = d
                  Tuple = t })
            ("int" := dint)
            ("float" := dfloat)
            ("decimal" := ddecimal)
            ("tuple" :=
                tuple2
                    (fun i s -> i, s)
                    dint
                    (oneOf [ dnull null; dstring ]))

    let decodeList =
        dlist decodeListObject

    let decodeMapObject =
        object1
            (fun list -> { MapObject.List = list })
            ("list" := decodeList)

    let decodeMap =
        dmap decodeMapObject
        |> map (Map.fold (fun (nmap: Map<MapKey, MapObject>) key value -> nmap.Add(Key key, value)) Map.empty)

    let decodeObject =
        object5 (fun name number maybeNumber list map ->
            { Name = name
              Number = number
              MaybeNumber = maybeNumber
              List = list
              Map = map })
            ("name" := oneOf [ dnull null; dstring ])
            ("number" := dint)
            ("maybeNumber" := maybe dint)
            ("list" := decodeList)
            ("map" := decodeMap)

    let rec decodeUnion =
        ("type" := dstring) >>= function
            | "single" -> succeed Single
            | "record" -> object1 Record decodeObject
            | "complex" ->
                object2
                    (fun n xs -> Complex (n, xs))
                    ("number" := dint)
                    ("list" := decodeList)
            | "recursive" ->
                object1 Recursive ("data" := decodeUnion)
            | _ -> invalidOp ""

module Generators =
    let genKey =
        Arb.generate<string>
        |> Gen.filter (not << isNull)
        |> Gen.map Key

    let genFloat =
        Arb.generate<NormalFloat>
        |> Gen.map (fun x -> System.Math.Round(float x, 8))

type Generators =
    static member MapKey(): Arbitrary<MapKey> =
        Arb.fromGen Generators.genKey
    static member Float(): Arbitrary<float> =
        Arb.fromGen Generators.genFloat

[<Property(Arbitrary = [| typeof<Generators> |])>]
let ``serializing a value and then deserializing from json should result in the same value`` (original: Union) =
    let res =
        Encoders.encodeUnion original
        |> encode false
        |> decodeString Decoders.decodeUnion
    res = Ok original
