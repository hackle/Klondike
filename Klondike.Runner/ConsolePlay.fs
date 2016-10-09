module ConsolePlay

open Klondike
open Components
open System
open FSharp.Text.RegexProvider
open PrettyPrint

let takeCommand game =
    printfn "Next move?"
    Console.ReadLine()

let fromStockToDiscard command game =
    match command with
    | "s2d" -> Move.fromStockToDiscard game
    | _ -> game
    
type F2TMatcher = Regex< @"^d2t(?<PileNumber>\d)$" >
let fromDiscardToTableau command game =
    let matcher = F2TMatcher()
    if matcher.IsMatch command
    then
        let m = F2TMatcher().TypedMatch command
        Move.fromDiscardToTableau (int m.PileNumber.Value) game
    else game

type T2FMatcher = Regex< @"^t(?<PileNumber>\d)2f$">
let fromTableauToFoundations command game =
    let matcher = T2FMatcher()
    if matcher.IsMatch command
    then
        let m = T2FMatcher().TypedMatch command
        Move.fromTableauToFoundation (int m.PileNumber.Value) game
    else game
    
type T2TMatcher = Regex< @"^t(?<PileNumber1>\d)2t(?<PileNumber2>\d)$">
let fromTableauToTableau command game =
    let matcher = T2TMatcher()
    if matcher.IsMatch command
    then
        let m = T2TMatcher().TypedMatch command
        Move.fromTableauToTableau (int m.PileNumber1.Value) (int m.PileNumber2.Value) game
    else game
    
let fromDiscardToFoundations command game =
    match command with
    | "d2f" ->
        Move.fromDiscardToFoundation game
    | _ -> game
    
let exitGame command game =
    match command with
    | "exit" -> None
    | _ -> Some game

type PlaysBuilder() =
    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x
    member this.Bind(o, f) = Option.bind f o

let plays = PlaysBuilder()

let makePlay game command =
    [ fromStockToDiscard; 
        fromDiscardToTableau;
        fromTableauToTableau;
        fromDiscardToFoundations;
        fromTableauToFoundations
    ]
    |> List.map (fun m -> m command)
    |> List.map Option.map
    |> List.append [ exitGame command |> Option.bind ]
    |> List.fold (fun carry current -> current carry) (Some game)
    
let printCommands() =
    printfn @"Commands: 
        s2d (Stock to Discard) 
        d2f (Discard to Foundations)
        d2t{PileNumber} (Discard to Tableau Pile)
        t{PileNumber1}2t{PileNumber2} (From one Tableau Pile to another)
        t{PileNumber}2f (From Tableau Pile to Foundations)"
    
let rec play game =
    Console.Clear()
    prettyPrint game
    printCommands()

    plays {
        let! g = takeCommand() |> makePlay game
        
        return! play g
    }