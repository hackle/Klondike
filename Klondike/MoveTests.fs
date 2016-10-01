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

// from discard to tableau

[<Fact>]
let ``Move from discard to tableau, remain unchanged if discard is empty`` () =
    let king = { Suit = Spade; Face = Face.King }
    let pile = TableauPile [ king ]
    let original = 
        {
            Discard = []; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [ pile ] 
        }
    let result = 
        original
        |> Move.fromDiscardToTableau 0

    Assert.Equal(original, result)

[<Fact>]
let ``Move from discard to tableau, remain unchanged if unsuccessful`` () =
    let king = { Suit = Spade; Face = Face.King }
    // this can't be added because they are of the same suit
    let queen = { Suit = Spade; Face = Face.Queen }
    let pile = TableauPile [ king ]
    let original = 
        {
            Discard = [ queen ]; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [ pile ] 
        }
    let result = 
        original
        |> Move.fromDiscardToTableau 0

    Assert.Equal(original, result)

[<Fact>]
let ``Move from discard to tableau, transfers card across`` () =
    let king = { Suit = Spade; Face = Face.King }
    let queen = { Suit = Diamond; Face = Face.Queen }
    let pile = TableauPile [ king ]
    let original = 
        {
            Discard = [ queen ]; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [ pile ] 
        }
    let result = 
        original
        |> Move.fromDiscardToTableau 0

    Assert.Equal<Card list>([ queen; king ], result.Tableau.[0].Value)
    Assert.True(result.Discard.IsEmpty)

// Move from tableau to tableau
[<Fact>]
let ``Move from tableau to tableau, remain unchanged if tableau1 is empty`` () =
    let king = { Suit = Spade; Face = Face.King }
    let pile1 = TableauPile [ ]
    let pile2 = TableauPile [ king ]

    let original = 
        {
            Discard = []; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [ pile1; pile2 ] 
        }

    let actual = original |> Move.fromTableauToTableau 0 1

    Assert.Equal<Set>(original, actual)

[<Fact>]
let ``Move from tableau to tableau, remain unchanged if unsuccessful`` () =
    let king = { Suit = Spade; Face = Face.King }
    // this won't work
    let queen = { Suit = Spade; Face = Face.Queen }
    let pile1 = TableauPile [ queen ]
    let pile2 = TableauPile [ king ]

    let original = 
        {
            Discard = []; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [ pile1; pile2 ] 
        }

    let actual = original |> Move.fromTableauToTableau 0 1

    Assert.Equal<Set>(original, actual)

[<Fact>]
let ``Move from tableau to tableau, transfers card across`` () =
    let king = { Suit = Spade; Face = Face.King }
    let queen = { Suit = Heart; Face = Face.Queen }
    let pile1 = TableauPile [ queen ]
    let pile2 = TableauPile [ king ]

    let original = 
        {
            Discard = []; 
            Foundations = Foundations.New()
            Stock = [];
            Tableau = [ pile1; pile2 ] 
        }

    let actual = original |> Move.fromTableauToTableau 0 1

    Assert.Equal<Card list>([ queen; king ], actual.Tableau.[1].Value)
    Assert.True(actual.Tableau.[0].Value.IsEmpty)