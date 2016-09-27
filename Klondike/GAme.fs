﻿namespace Klondike

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

    type Card = Card of Suit * CardNumber
    type Foundations = {
        Diamond: Card list;
        Club: Card list;
        Heart: Card list;
        Spade: Card list;
    }
        

    type Set = { 
        Tableau: Card list list; 
        Stock: Card list;
        Discard: Card list;
        Foundations: Foundations
    }

    let getFoundationCards suit set =

    let getAllCards () =
        let allSuits = allUnionCases<Suit>()
        let allNumbers = allUnionCases<CardNumber>()
        seq {
            for s in allSuits do
            for n in allNumbers do
            yield Card (s, n)
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
            Foundations = { Diamond = []; Club = []; Heart = []; Spade = [] }
        }

    let transfer set = 
        let nextStock = if set.Stock = [] then set.Discard else set.Stock
        let nextDiscard = if set.Stock = [] then [] else set.Discard
        { set with Discard = nextDiscard; Stock = nextStock }

    let pickFromStock set =
        { 
            set with 
                Discard = (List.head set.Stock)::set.Discard;
                Stock = (List.tail set.Stock) 
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

    [<Fact>]
    let ``If the Stock becomes empty, turn the entire discard pile over and make it the new Stock.`` () =
        let stock = getAllCards() |> List.ofSeq |> List.take 5
        let next = 
            {
                Tableau = [];
                Stock = [];
                Discard = stock;
                Foundations = []
            }
            |> transfer

        Assert.Equal<Card list>(next.Stock, stock)

    [<Fact>]
    let ``Turn over the top card of the Stock and place it face-up on the Discard pile`` () =
        let (stock, discard) = 
            getAllCards() 
            |> List.ofSeq 
            |> List.take 10
            |> List.splitAt 5
        let next = 
            {
                Tableau = [];
                Stock = stock;
                Discard = discard;
                Foundations = []
            }
            |> pickFromStock

        Assert.Equal<Card list>(next.Stock, stock |> List.tail)
        Assert.Equal<Card list>(next.Discard, (List.head stock) :: discard)

    [<Fact>]
    let ``Move to foundation, if foundation is empty then only Ace is allowed`` () =
        let next = 
            {
                Tableau = [];
                Stock = [];
                Discard = [];
                Foundations = []
            }
            |> addToFoundation Spade (Card Spade, Ace)

        let actual = next.Foundations.Spade |> List.head

        Assert.Equal<Card>((Card Spade, Ace), actual)
