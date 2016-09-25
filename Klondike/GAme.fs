namespace Klondike

module Game =
    open Microsoft.FSharp.Reflection

    let allUnionCases<'a>() = 
        FSharpType.GetUnionCases typeof<'a>
        |> Array.map (fun u -> FSharpValue.MakeUnion u,[||] :?> 'a)

    type CardNumber = 
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | J
        | Q
        | K
        | A

    type Suit = 
        | Diamond
        | Club
        | Heart
        | Spade

    type Card = Suit * CardNumber
    type Pile = Pile of Card list
    type Foundation = Suit * Pile

    type Set = { 
        Tableau: Pile list; 
        Stock: Pile;
        Discard: Pile;
        Foundations: Foundation list 
    }

    let getAllCards () =
        let allSuits = FSharpType.GetUnionCases typeof<Suit>
        let allNumbers = FSharpType.GetUnionCases typeof<CardNumber>
        seq {
            for s in allSuits do
            for n in allNumbers do
            yield FSharpType, n
        }        

    let deal () = 
        {
            Tableau = List.init 7 (fun i -> Pile [] );
            Stock = Pile [];
            Discard = Pile [];
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
            let pile = set.Tableau |>List.item (idx - 1)
            let (Pile cards) = pile
            Assert.Equal(idx, cards |> List.length)
        [ 1 .. 7 ]
        |> List.iter assertItemAt