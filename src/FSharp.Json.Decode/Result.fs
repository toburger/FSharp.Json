[<AutoOpen>]
module Prelude

type Result<'a, 'b> =
    | Err of 'a
    | Ok of 'b

let bind binder res =
    match res with
    | Ok v -> binder v
    | Err e -> Err e

let returnM = Ok

type ResultBuilder() =
    member __.Bind(m, f) = bind f m
    member __.Return(v) = returnM v

let result = ResultBuilder()

module Result =

    let succeed = Ok

    let fail = Err

    let map mapping res =
        match res with
        | Ok v -> Ok (mapping v)
        | Err e -> Err e

    let bind = bind

    let map2 mapping res1 res2 = result {
        let! a = res1
        let! b = res2
        return mapping a b
    }

    let map3 mapping res1 res2 res3 = result {
        let! a = res1
        let! b = res2
        let! c = res3
        return mapping a b c
    }

    let map4 mapping res1 res2 res3 res4 = result {
        let! a = res1
        let! b = res2
        let! c = res3
        let! d = res4
        return mapping a b c d
    }

    let mapError (mapping: 'x -> 'error) res =
        match res with
        | Err err -> Err (mapping err)
        | Ok v -> Ok v

    let transform (ls: list<Result<'a, 'b>>) : Result<list<'a>, list<'b>> =
        let succs, errs = List.partition (function Ok _ -> true | Err _ -> false) ls
        match succs, errs with
        | succs, [] -> Ok (succs |> List.choose (function Ok v -> Some v | Err _ -> None))
        | _, errs -> Err (errs |> List.choose (function Err err -> Some err | Ok _ -> None))

    let withDefault (a: 'a) (result: Result<'x, 'a>) : 'a =
        match result with
        | Ok v -> v
        | Err _ -> a

    let toOption (result: Result<'x, 'a>) : 'a option =
        match result with
        | Ok v -> Some v
        | Err _ -> None

    let ofOption (x: 'x) (option: 'a option) : Result<'x, 'a> =
        match option with
        | Some v -> Ok v
        | None -> Err x
