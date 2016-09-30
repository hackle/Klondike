module ComponentTests

open Xunit
open Klondike.Components

[<Fact>]
let ``Add to foundations, if foundation is empty then only Ace is allowed`` () =
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
