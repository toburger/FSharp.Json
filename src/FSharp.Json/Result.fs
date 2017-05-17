[<RequireQualifiedAccess>]
module Result

let fail x = Error x

let ok x = Ok x

let either okf errf result =
    match result with
    | Ok x -> okf x
    | Error err -> errf err

let apply fr xr =
    fr
    |> Result.bind (fun f ->
        xr
        |> Result.map (fun x -> f x))

let collect (list: list<Result<'T, 'Err>>): Result<list<'T>, 'Err> =
    let (<*>) = apply
    let retn = Ok
    let cons head tail = head :: tail
    let initState = retn []
    let folder head tail =
        retn cons <*> head <*> tail
    List.foldBack folder list initState

let map2 f res1 res2 =
    res1 |> Result.bind (fun x ->
        res2 |> Result.map (fun y -> f x y))

let map3 f res1 res2 res3 =
    res1 |> Result.bind (fun x ->
        res2 |> Result.bind (fun y ->
            res3 |> Result.map (fun z -> f x y z)))

let returnOrFail = function
    | Ok x -> x
    | Error msg -> fail msg
