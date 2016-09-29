namespace Klondike

module GameOps =
    open Components

    type Foundation with
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
    
    type Foundations with
        static member New () = 
            { 
                Diamond = { Suit = Diamond; Cards = [] };
                Club = { Suit = Club; Cards = [] };
                Heart = { Suit = Heart; Cards = [] };
                Spade = { Suit = Spade; Cards = [] }
            }

        static member add (card: Card) foundations =
            match card.Suit with
            | Diamond -> { foundations with Diamond = foundations.Diamond |> Foundation.add card  }
            | Club -> { foundations with Club = foundations.Club |> Foundation.add card }
            | Heart -> { foundations with Heart = foundations.Heart |> Foundation.add card }
            | Spade -> { foundations with Spade = foundations.Spade |> Foundation.add card }

    type TableauPile with
        static member add (card: Card) (pile: TableauPile) =
            let currentMaxValue = 
                if pile.Value.IsEmpty then
                    EnumHelper.allValues<Face>()
                    |> Seq.max
                    |> int
                    |> (+) 1

                else pile.Value.Head.Face |> int

            let canAdd = (int card.Face) = currentMaxValue - 1

            match canAdd with
            | false -> pile
            | true -> TableauPile (card::pile.Value)