namespace Klondike

module Game =
    open Microsoft.FSharp.Reflection

    let allUnionCases<'a>() = 
        FSharpType.GetUnionCases typeof<'a>
        |> Array.map (fun u -> FSharpValue.MakeUnion(u, [||]) :?> 'a)

    type CardNumber =     
        | Ace
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | Jack
        | Queen
        | King

    type Suit = 
        | Diamond
        | Club
        | Heart
        | Spade

    type Card = Suit * CardNumber
    type Foundation = Suit * Card list

    type Set = { 
        Tableau: Card list list; 
        Stock: Card list;
        Discard: Card list;
        Foundations: Foundation list 
    }

    let getAllCards () =
        let allSuits = allUnionCases<Suit>()
        let allNumbers = allUnionCases<CardNumber>()
        seq {
            for s in allSuits do
            for n in allNumbers do
            yield s, n
        }

    let permute list =
        let rand = new System.Random()
        let rec permute' remaining result =
            match remaining with
            | [] -> result
            | _ ->
                let index = rand.Next(0, (List.length remaining))
                let item = remaining |> List.item index
                permute' (remaining |> List.except [ item ]) (item::result)
        permute' list []
        
    let takeFromList count list = 
        List.splitAt count

    let deal () = 
        let allCards = getAllCards() |> List.ofSeq |> permute
        let folder carry count =
            let (piles, rest) = carry
            let (take, leave) = List.splitAt count rest
            take :: piles, leave

        let (tableau, undealt) =
            [ 7 .. -1 .. 1 ]
            |> List.fold folder ([], allCards)

        {
            Tableau = tableau;
            Stock = undealt;
            Discard = [];
            Foundations = []
        }

module DealerTests =
    open Xunit
    open Game

    [<Fact>]
    let ``Each set should have 7 piles``() =
        let set = deal()
        Assert.Equal(set.Tableau|>List.length, 7)

    [<Fact>]
    let ``First pile has 1 card, 2nd 2 cards and so on`` () =
        let set = deal()
        let assertItemAt idx = 
            let cards = set.Tableau |>List.item (idx - 1)
            Assert.Equal(idx, cards |> List.length)
        [ 1 .. 7 ]
        |> List.iter assertItemAt