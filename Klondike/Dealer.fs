namespace Klondike

[<RequireQualifiedAccess>]
module Dealer =
    type Card = 
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
    type Suit = Suit of Card list
    type Pile = Pile of Card list
    type Set = { Piles: Pile list }

    let deal () = 
        { Piles = List.init 7 (fun i -> Pile [] ) }

module DealerTests =
    open Xunit

    [<Fact>]
    let ``Each set should have 7 piles``() =
        let set = Dealer.deal()
        Assert.Equal(set.Piles|>List.length, 7)

