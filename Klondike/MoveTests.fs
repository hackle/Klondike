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

[<Fact>]
let ``Move from discard to foundation, remains same if discard is empty`` () =
    let ace = { Suit = Spade; Face = Face.Ace }
    let set =
        {
            Discard = []; 
            Foundations = Foundations.New() |> Foundations.add ace
            Stock = [];
            Tableau = [] 
        }
        |> Move.fromDiscardToFoundation

    Assert.Equal<Card list>(set.Discard, [])
    Assert.Equal<Card>(ace, set.Foundations.Spade.Cards.Head)

[<Fact>]
let ``Move from discard to foundation, remains same if unsuccessful`` () =
    // this can't be added because a foundation must start with Ace
    let disallowed = { Suit = Spade; Face = Face.Two }
    let set =
        {
            Discard = []; 
            Foundations = Foundations.New() |> Foundations.add disallowed
            Stock = [];
            Tableau = [] 
        }
        |> Move.fromDiscardToFoundation

    Assert.Equal<Card list>([], set.Discard)
    Assert.Equal<Card list>([], set.Foundations.Spade.Cards)

[<Fact>]
let ``Move from discard to foundation, transfers head if successful`` () =
    let ace = { Suit = Spade; Face = Face.Ace }
    let set =
        {
            Discard = [ ace ]; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [] 
        }
        |> Move.fromDiscardToFoundation

    Assert.Equal<Card list>(set.Discard, [])
    Assert.Equal<Card>(ace, set.Foundations.Spade.Cards.Head)

// move from tableau to foundation
[<Fact>]
let ``Move from tableau to foundation, remains same if discard is empty`` () =
    let ace = { Suit = Spade; Face = Face.Ace }
    let pile = TableauPile [ ace ]
    let set =
        {
            Discard = []; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [ pile ] 
        }
        |> Move.fromTableauToFoundation 0

    Assert.Equal<Card list>(set.Discard, [])
    Assert.Equal<Card>(ace, set.Foundations.Spade.Cards.Head)

[<Fact>]
let ``Move from tableau to foundation, remains same if unsuccessful`` () =
    // this can't be added because a foundation must start with Ace
    let disallowed = { Suit = Spade; Face = Face.Two }
    let pile = TableauPile [ disallowed ]
    let set =
        {
            Discard = []; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [ pile ] 
        }
        |> Move.fromTableauToFoundation 0

    Assert.Equal<Card list>([ disallowed ], set.Tableau.[0].Value)
    Assert.Equal<TableauPile>(pile, set.Tableau.[0])

[<Fact>]
let ``Move from tableau to foundation, transfers head if successful`` () =
    let ace = { Suit = Spade; Face = Face.Ace }
    let pile = TableauPile [ ace ]
    let set =
        {
            Discard = []; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [ pile ] 
        }
        |> Move.fromTableauToFoundation 0

    Assert.True(set.Tableau.[0].Value.IsEmpty)
    Assert.Equal<Card>(ace, set.Foundations.Spade.Cards.Head)