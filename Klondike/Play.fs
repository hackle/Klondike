namespace Klondike

module parts =
    open Game

    type Foundation with
        static member add (card: Card) foundation = 
            let currentMaxVal = 
                if foundation.Cards.IsEmpty then 
                    EnumHelper.allValues<Face>()
                    |> Seq.min
                    |> int

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
                else pile.Value.Head.Face |> int

            let canAdd = (int card.Face) = currentMaxValue - 1

            match canAdd with
            | false -> pile
            | true -> TableauPile (card::pile.Value)

[<RequireQualifiedAccess>]
module Play =
    
    open Game
    open ListExtensions
    
    type Result<'s, 'f> =
        | Success of 's
        | Failure of 'f

    let deal () = 
        let allCardsShuffled = AllCards |> List.ofSeq |> List.shuffle
        let folder carry count =
            let (piles, rest) = carry
            let (take, leave) = List.splitAt count rest
            (TableauPile take) :: piles, leave

        let (tableau, undealt) =
            [ 7 .. -1 .. 1 ]
            |> List.fold folder ([], allCardsShuffled)

        {
            Tableau = tableau;
            Stock = undealt;
            Discard = [];
            Foundations = Foundations.New()
        }
    
    let addToFoundation card set = 
        { 
            set with 
                Foundations = set.Foundations |> Foundations.add card 
        }

    let update set = 
        let nextStock = if set.Stock = [] then set.Discard else set.Stock
        let nextDiscard = if set.Stock = [] then [] else set.Discard
        { 
            set with 
                Discard = nextDiscard; 
                Stock = nextStock 
        }

    let pickFromStock set =
        { 
            set with 
                Discard = (List.head set.Stock)::set.Discard;
                Stock = (List.tail set.Stock) 
        }

