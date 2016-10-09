// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Klondike
open Components
open System

let inline takeOne obj =
    match (sprintf "%A" obj) with
    | "" -> ""
    | x -> x.Substring(0, 1)

let formatCard (card: Card) =
    sprintf "(%s %A)" (card.Suit |> takeOne) card.Face

let formatCards (cards: Card list) =
    cards 
    |> List.map (fun c -> formatCard c)
    |> (fun segs -> match segs with 
                    | [] -> String.Empty
                    | _ -> List.reduce (fun c1 c2 -> c1 + " " + c2) segs)

let printCards (cards: Card list) =
    cards
    |> formatCards
    |> printfn "%s"

let printFoundations (foundations: Foundations) =
    let printFoundation' (foundation: Foundation) =
        foundation.Cards |> formatCards
        |> printfn "%A: %s" foundation.Suit

    [ foundations.Club;
        foundations.Diamond;
        foundations.Heart;
        foundations.Spade ]
    |> List.iter printFoundation'
    
let printPile idx (p: TableauPile) =
    printfn "%i - %s" (idx + 1) (p.Value |> formatCards)
   
let prettyPrint (game: Game) =
    printfn "Tableau"
    game.Tableau |> List.iteri printPile

    printfn "Foundations"
    game.Foundations |> printFoundations

    printfn "Stock"
    game.Stock |> printCards

    printfn "Discarded"
    game.Discard |> printCards

let takeCommand game =
    printfn "Next move?"
    Console.ReadLine()

let fromStockToDiscard command game =
    match command with
    | "s2d" -> Move.fromStockToDiscard game
    | _ -> game
    
let makePlay game raw =
    [ fromStockToDiscard ]
    |> List.fold (fun carry current -> current raw carry) game
    
let rec play game =
    Console.Clear()
    prettyPrint game

    takeCommand()
    |> makePlay game
    |> play

[<EntryPoint>]
let main argv = 
    Play.deal()
    |> play
    |> ignore

    Console.ReadKey() |> ignore
    0 // return an integer exit code
