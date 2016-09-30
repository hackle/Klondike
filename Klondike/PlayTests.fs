module PlayTests
open Xunit
open Klondike
open Klondike.Components

[<Fact>]
let ``Each set should have 7 piles``() =
    let set = Play.deal()
    Assert.Equal(set.Tableau|>List.length, 7)

[<Fact>]
let ``First pile has 1 card, 2nd 2 cards and so on`` () =
    let set = Play.deal()
    let assertItemAt idx = 
        let pile = set.Tableau |>List.item (idx - 1)
        Assert.Equal(idx, pile.Value |> List.length)
    [ 1 .. 7 ]
    |> List.iter assertItemAt

[<Fact>]
let ``If the Stock becomes empty, turn the entire discard pile over and make it the new Stock.`` () =
    let stock = Card.AllCards |> List.ofSeq |> List.take 5
    let next = 
        {
            Tableau = [];
            Stock = [];
            Discard = stock;
            Foundations = Foundations.New()
        }
        |> Play.update

    Assert.Equal<Card list>(next.Stock, stock)

[<Fact>]
let ``Turn over the top card of the Stock and place it face-up on the Discard pile`` () =
    let (stock, discard) = 
        Card.AllCards
        |> List.ofSeq 
        |> List.take 10
        |> List.splitAt 5
    let next = 
        {
            Tableau = [];
            Stock = stock;
            Discard = discard;
            Foundations = Foundations.New()
        }
        |> Play.pickFromStock

    Assert.Equal<Card list>(next.Stock, stock |> List.tail)
    Assert.Equal<Card list>(next.Discard, (List.head stock) :: discard)

[<Fact>]
let ``Move to foundation, if foundation is empty then only Ace is allowed`` () =
    let card = { Suit = Spade; Face = Face.Ace }
    let next = 
        {
            Tableau = [];
            Stock = [];
            Discard = [];
            Foundations = Foundations.New() |> Foundations.add card
        } 
        |> Play.addToFoundation card

    let actual = next.Foundations.Spade.Cards |> List.head

    Assert.Equal<Card>(card, actual)

[<Fact>]
let ``Move to foundation, if foundation is empty then none-Ace is not allowed`` () =    
    let card = { Suit = Spade; Face = Face.Two }
    let next = 
        {
            Tableau = [];
            Stock = [];
            Discard = [];
            Foundations = Foundations.New()
        }
        |> Play.addToFoundation card

    let actual = next.Foundations.Spade.Cards

    Assert.Equal<Card>([], actual)
        
[<Fact>]
let ``Move to foundation, can increment by one`` () =    
    let ace = { Suit = Spade; Face = Face.Ace }
    let two = { Suit = Spade; Face = Face.Two }
    let next = 
        {
            Tableau = [];
            Stock = [];
            Discard = [];
            Foundations = 
                Foundations.New() 
                |> Foundations.add ace
        }
        |> Play.addToFoundation two

    let actual = next.Foundations.Spade.Cards.Head

    Assert.Equal<Card>(two, actual)

[<Fact>]
let ``Move to foundation, cannot increment by more than one`` () =    
    let ace = { Suit = Spade; Face = Face.Ace }
    let three = { Suit = Spade; Face = Face.Three }
    let next = 
        {
            Tableau = [];
            Stock = [];
            Discard = [];
            Foundations = 
                Foundations.New() 
                |> Foundations.add ace
        }
        |> Play.addToFoundation three

    let actual = next.Foundations.Spade.Cards.Head

    Assert.Equal<Card>(ace, actual)