namespace Klondike

[<RequireQualifiedAccess>]
module Dealer =
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

    type Game = { 
        Tableau: Pile list; 
        Stock: Pile;
        Discard: Pile;
        Foundations: Foundation list 
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

    [<Fact>]
    let ``Each set should have 7 piles``() =
        let set = Dealer.deal()
        Assert.Equal(set.Tableau|>List.length, 7)

