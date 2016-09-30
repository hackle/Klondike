﻿module ComponentTests

open Xunit
open Klondike.Components

[<Fact>]
let ``Add to tableaus, if foundation is empty then only Ace is allowed`` () =
    let card = { Suit = Spade; Face = Face.Ace }
    let next = Foundations.New() |> Foundations.add card

    let actual = next.Spade.Cards |> List.head

    Assert.Equal<Card>(card, actual)

[<Fact>]
let ``Add to foundations, if foundation is empty then none-Ace is not allowed`` () =    
    let card = { Suit = Spade; Face = Face.Two }
    let next = Foundations.New() |> Foundations.add card

    let actual = next.Spade.Cards

    Assert.Equal<Card>([], actual)
        
[<Fact>]
let ``Add to foundations, can increment by one`` () =    
    let ace = { Suit = Spade; Face = Face.Ace }
    let two = { Suit = Spade; Face = Face.Two }
    let next = 
        Foundations.New() 
        |> Foundations.add ace
        |> Foundations.add two

    let actual = next.Spade.Cards.Head

    Assert.Equal<Card>(two, actual)

[<Fact>]
let ``Add to foundations, cannot increment by more than one`` () =    
    let ace = { Suit = Spade; Face = Face.Ace }
    let three = { Suit = Spade; Face = Face.Three }
    let next = 
        Foundations.New()
        |> Foundations.add ace
        |> Foundations.add three

    let actual = next.Spade.Cards.Head

    Assert.Equal<Card>(ace, actual)

// Add to tableau pile

[<Fact>]
let ``Add to tableau, if pile is empty then King is allowed`` () =
    let card = { Suit = Spade; Face = Face.King }
    let next = 
        (TableauPile []) |> TableauPile.add card

    let actual = next.Value |> List.head

    Assert.Equal<Card>(card, actual)

[<Fact>]
let ``Add to tableau, if pile is empty then non-King is not allowed`` () =
    let card = { Suit = Spade; Face = Face.Queen }
    let next = 
        (TableauPile []) |> TableauPile.add card
        
    Assert.Equal<Card>(next.Value, [])
    
[<Fact>]
let ``Add to tableau, can add if decrement face and alternate color`` () =
    let blackKing = { Suit = Heart; Face = Face.King }
    let redQueen = { Suit = Spade; Face = Face.Queen }
    let next = 
        (TableauPile []) 
        |> TableauPile.add blackKing
        |> TableauPile.add redQueen
        
    Assert.Equal<Card>(next.Value, [ redQueen; blackKing ])
    
[<Fact>]
let ``Add to tableau, can not add if decrement face but with same color`` () =
    let redKing = { Suit = Heart; Face = Face.King }
    let redQueen = { Suit = Diamond; Face = Face.Queen }
    let next = 
        (TableauPile []) 
        |> TableauPile.add redKing
        |> TableauPile.add redQueen
        
    Assert.Equal<Card>(next.Value, [ redKing ])

[<Fact>]
let ``Add to tableau, can add if decrement face with different color`` () =
    let redKing = { Suit = Heart; Face = Face.King }
    let blackQueen = { Suit = Club; Face = Face.Queen }
    let next = 
        (TableauPile []) 
        |> TableauPile.add redKing
        |> TableauPile.add blackQueen
        
    Assert.Equal<Card>(next.Value, [ blackQueen; redKing ])
//[<Fact>]
//let ``Add to tableau, if tableau is empty then none-Ace is not allowed`` () =    
//    let card = { Suit = Spade; Face = Face.Two }
//    let next = Tableau.New() |> Tableau.add card
//
//    let actual = next.Spade.Cards
//
//    Assert.Equal<Card>([], actual)
//        
//[<Fact>]
//let ``Add to tableau, can increment by one`` () =    
//    let ace = { Suit = Spade; Face = Face.Ace }
//    let two = { Suit = Spade; Face = Face.Two }
//    let next = 
//        Tableau.New() 
//        |> Tableau.add ace
//        |> Tableau.add two
//
//    let actual = next.Spade.Cards.Head
//
//    Assert.Equal<Card>(two, actual)
//
//[<Fact>]
//let ``Add to tableau, cannot increment by more than one`` () =    
//    let ace = { Suit = Spade; Face = Face.Ace }
//    let three = { Suit = Spade; Face = Face.Three }
//    let next = 
//        Tableau.New()
//        |> Tableau.add ace
//        |> Tableau.add three
//
//    let actual = next.Spade.Cards.Head
//
//    Assert.Equal<Card>(ace, actual)
