module Isomorphism

open FsCheck
open FsCheck.NUnit
open FSharp.Json.Encode
open FSharp.Json.Decode

type MapKey = Key of string

type ListObject = {
    Number: float
}

type MapObject = {
    List: ListObject list
}

type Object = {
    Name: string
    Number: int
    MaybeNumber: int option
    List: ListObject list
    Map: Map<MapKey, MapObject>
}

type Union =
    | Object of Object
    | Number of int

module Encoders =
    let encodeListObject (o: ListObject) =
        jobject [ "number", jfloat o.Number ]

    let encodeList =
        jlist << List.map encodeListObject

    let encodeMapObject (o: MapObject) =
        jobject [ "list", encodeList o.List ]

    let encodeMap (map: Map<_, _>) =
        Seq.map (fun (KeyValue(Key k, v)) -> k, encodeMapObject v) map
        |> jobject
        
    let encodeObject o =
        [ "name",  if o.Name = null then jnull else jstring o.Name
          "number", jint o.Number
          "maybeNumber", o.MaybeNumber |> Option.map jint |> Option.defaultValue jnull
          "list", encodeList o.List
          "map", encodeMap o.Map ]

    let encodeUnion = function
        | Object o -> jobject (("type", jstring "object") :: encodeObject o)
        | Number n -> jobject [ "type", jstring "number"; "n", jint n ]

module Decoders =
    let decodeListObject =
        object1
            (fun number -> { ListObject.Number = number })
            ("number" := dfloat)

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
            { Name = name; Number = number; MaybeNumber = maybeNumber; List = list; Map = map })
            ("name" := oneOf [ dnull null; dstring ])
            ("number" := dint)
            ("maybeNumber" := maybe dint)
            ("list" := decodeList)
            ("map" := decodeMap)

    let decodeUnion =
        ("type" := dstring) >>= function
            | "object" -> object1 Object decodeObject
            | "number" -> object1 Number ("n" := dint)
            | _ -> invalidOp ""

module Generators =
    let genAlphanumString = gen {
        let validChars = ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']
        let! chars = Gen.nonEmptyListOf (Gen.oneof (List.map Gen.constant validChars))
        return System.String(List.toArray chars)
    }
    
    let genFloat =
        Arb.generate<NormalFloat>
        |> Gen.map (fun x -> System.Math.Round(float x, 10))

type Generators =
    static member String(): Arbitrary<string> =
        Arb.fromGen Generators.genAlphanumString
    static member MapKey(): Arbitrary<MapKey> =
        Arb.fromGen <| Gen.map Key Generators.genAlphanumString
    static member Float(): Arbitrary<float> =
        Arb.fromGen Generators.genFloat

[<Property(Arbitrary = [| typeof<Generators> |])>]
let testIsomorphism (original: Union) =
    let res =
        Encoders.encodeUnion original
        |> encode false
        |> decodeString Decoders.decodeUnion
    res = Ok original
