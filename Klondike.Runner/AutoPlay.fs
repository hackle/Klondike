module AutoPlay

open Klondike.Components
open Klondike
open Maybe
open PrettyPrint

let indice = [ 0 .. 6 ]
let moves =
    [| for g1' in indice do
        for g2' in indice do
        if g1' <> g2' then
            yield g1', g2'
    |]
    |> List.ofSeq

let tryAllPiles game =

    let move' (m: int * int) = 
        Move.fromTableauToTableau (m |> fst) (m |> snd)
    
    let rec try' game' pasts' =
        // moved things off to foundations
        let cleared =
            indice
            |> List.fold (fun g i -> Move.fromTableauToFoundation i g) game'
                
        // now move between the piles
        let moved = 
            moves
            |> List.fold (fun g m -> move' m g) cleared
    
        // until no more moves are possible, 
        if List.contains moved pasts'
        then moved
        else try' moved (moved::pasts')

    try' game [ game ]
//
//let moveFromStockToDiscardSafe game =
//    // move twice when stock is empty because the 1st move 
//    match game.Stock with
//    | [] -> game |> (Move.fromStockToDiscard >> Move.fromStockToDiscard)
//    | _ -> Move.fromStockToDiscard game

let tryStock game =
    let cleared =
        game
        |> Move.fromStockToDiscard
        |> Move.fromDiscardToFoundation

    indice
    |> List.fold (fun g i -> Move.fromDiscardToTableau i g) cleared

let rec autoPlayOne game pasts waiter =
    System.Console.Clear()
    prettyPrint game
    waiter()

    let g' = 
        [ tryAllPiles; tryStock ]
        |> List.fold (fun carry current -> current carry) game

    // nothing has changed, no more moves possible
    if List.contains g' pasts
    then 
        printfn "This is my wits' end"
        g' 
    else autoPlayOne g' (g'::pasts) waiter

let autoplay game =
    let waiter () = 
        printfn "And next move..."
        System.Threading.Thread.Sleep(500)
    autoPlayOne game [ game ] waiter