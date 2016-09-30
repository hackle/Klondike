namespace Klondike

module ListExtensions =
    type List<'T> with
    static member shuffle list =
        let rand = new System.Random()
        let rec permute' remaining result =
            match remaining with
            | [] -> result
            | _ ->
                let index = rand.Next(0, (List.length remaining))
                let item = remaining |> List.item index
                permute' (remaining |> List.except [ item ]) (item::result)
        permute' list []

    static member replace a b list =
        list
        |> List.map
            (fun e -> if e = a then b else e)

[<RequireQualifiedAccess>]
module UnionCaseHelper = 
    
    open Microsoft.FSharp.Reflection
    
    let allUnionCases<'a>() = 
        FSharpType.GetUnionCases typeof<'a>
        |> Array.map (fun u -> FSharpValue.MakeUnion(u, [||]) :?> 'a)

[<RequireQualifiedAccess>]
module EnumHelper =
    let allValues<'a>()=
        seq { for e in System.Enum.GetValues(typeof<'a>) do yield e :?> 'a }