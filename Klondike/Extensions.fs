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

[<RequireQualifiedAccess>]
module UnionCaseHelper = 
    
    open Microsoft.FSharp.Reflection
    
    let AllUnionCases<'a>() = 
        FSharpType.GetUnionCases typeof<'a>
        |> Array.map (fun u -> FSharpValue.MakeUnion(u, [||]) :?> 'a)