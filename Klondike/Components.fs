﻿namespace Klondike

module Components =
    open ListExtensions
    
    type Face =
        | Ace = 1
        | Two = 2
        | Three = 3
        | Four = 4
        | Five = 5
        | Six = 6
        | Seven = 7
        | Eight = 8
        | Nine = 9
        | Ten = 10
        | Jack = 11
        | Queen = 12
        | King = 13

    type Suit = 
        | Diamond
        | Club
        | Heart
        | Spade

    type Color = | Red | Black

    type Card = { Suit: Suit; Face: Face } with
        static member AllCards =
            seq {
                for s in UnionCaseHelper.allUnionCases<Suit>() do
                for n in EnumHelper.allValues<Face>() do
                yield { Suit = s; Face = n }
            }
            |> List.ofSeq

        static member color card =
            match card.Suit with
            | Diamond | Heart -> Red
            | _ -> Black

    type Foundation = 
        { 
            Suit: Suit; 
            Cards: Card list 
        } with
        static member add (card: Card) foundation = 
            let currentMaxVal = 
                if foundation.Cards.IsEmpty then 
                    EnumHelper.allValues<Face>()
                    |> Seq.min
                    |> int
                    |> (-) 1

                else (int foundation.Cards.Head.Face)

            let isIncrementBy1 = (int card.Face) = currentMaxVal + 1

            let canAdd = 
                card.Suit = foundation.Suit && isIncrementBy1

            match canAdd with
            | true -> { foundation with Cards = card :: foundation.Cards }
            | false -> foundation
            
    type Foundations = 
        {
            Diamond: Foundation;
            Club: Foundation;
            Heart: Foundation;
            Spade: Foundation;
        } with
        static member New () = 
            { 
                Diamond = { Suit = Diamond; Cards = [] };
                Club = { Suit = Club; Cards = [] };
                Heart = { Suit = Heart; Cards = [] };
                Spade = { Suit = Spade; Cards = [] }
            }

        static member has (card: Card) foundations =
            match card.Suit with
            | Diamond -> foundations.Diamond.Cards |> List.contains card
            | Club -> foundations.Club.Cards |> List.contains card
            | Heart -> foundations.Heart.Cards |> List.contains card
            | Spade -> foundations.Spade.Cards |> List.contains card

        static member add (card: Card) foundations =
            match card.Suit with
            | Diamond -> { foundations with Diamond = foundations.Diamond |> Foundation.add card  }
            | Club -> { foundations with Club = foundations.Club |> Foundation.add card }
            | Heart -> { foundations with Heart = foundations.Heart |> Foundation.add card }
            | Spade -> { foundations with Spade = foundations.Spade |> Foundation.add card }

    type TableauPile = TableauPile of Card list
    with 
        member this.Value = 
            let (TableauPile cards) = this
            cards

        static member add (card: Card) (pile: TableauPile) =
            let currentMaxValue = 
                if pile.Value.IsEmpty 
                then
                    EnumHelper.allValues<Face>()
                    |> Seq.max
                    |> int
                    |> (+) 1
                else pile.Value.Head.Face |> int
            let faceValueIsDecrementBy1 = (int card.Face) = currentMaxValue - 1

            let nextColor =
                if pile.Value.IsEmpty
                then card |> Card.color
                else 
                    match pile.Value.Head |> Card.color with
                    | Red -> Black
                    | Black -> Red
            let colorMatches = nextColor = (card |> Card.color)
            
            let canAdd = faceValueIsDecrementBy1 && colorMatches

            match canAdd with
            | false -> pile
            | true -> TableauPile (card::pile.Value)
            
    type Set = { 
        Tableau: TableauPile list;
        Stock: Card list;
        Discard: Card list;
        Foundations: Foundations
    }