[<RequireQualifiedAccess>]
module Result

let ok x = Ok x

let fail x = Error x

let map2 f res1 res2 =
    res1 |> Result.bind (fun x ->
    res2 |> Result.map (fun y ->
    f x y))

let map3 f res1 res2 res3 =
    res1 |> Result.bind (fun x ->
    res2 |> Result.bind (fun y ->
    res3 |> Result.map (fun z ->
    f x y z)))

let apply fr xr =
    map2 (fun f x -> f x) fr xr

let collect list =
    let (<*>) = apply
    let cons head tail = head :: tail
    let initState = ok []
    let folder head tail =
        ok cons <*> head <*> tail
    List.foldBack folder list initState

let either okf errf result =
    match result with
    | Ok x -> okf x
    | Error err -> errf err

let returnOrFail result =
    either id failwith result
