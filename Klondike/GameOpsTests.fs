module GameOpsTests

open Xunit
open Klondike.Components
open Klondike

[<Fact>]
let ``Move from stock to discard, swaps if stock is empty`` () =
    let cards = Card.AllCards |> List.take 5
    let set = 
        { Stock = []; Discard = cards; Tableau = []; Foundations = Foundations.New() }
        |> Move.fromStockToDiscard

    Assert.Equal<Card list>(set.Stock, cards)
    Assert.Equal<Card list>(set.Discard, [])

[<Fact>]
let ``Move from stock to discard, takes one off stock and adds to discard`` () =
    let cards = Card.AllCards |> List.take 5
    let set =
        { Stock = cards; Discard = []; Tableau = []; Foundations = Foundations.New() }
        |> Move.fromStockToDiscard

    Assert.Equal<Card>(cards.Head, set.Discard.Head)
