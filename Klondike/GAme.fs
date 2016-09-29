namespace Klondike

module Game =
    open ListExtensions
    
    type Face =
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
        | Ace = 14

    type Suit = 
        | Diamond
        | Club
        | Heart
        | Spade

    type Card = { Suit: Suit; Face: Face }

    let AllCards =
        let allSuits = UnionCaseHelper.AllUnionCases<Suit>()
        let allFaces =
            [ (int Face.Two) .. (int Face.Ace) ]
            |> List.map (fun i -> enum i)

        seq {
            for s in allSuits do
            for n in allFaces do
            yield { Suit = s; Face = n }
        }

    type Foundation = 
        { 
            Suit: Suit; 
            Cards: Card list 
        } with
        static member add (card: Card) foundation = 
            let isAceOverEmpty () =
                foundation.Cards = [] && card.Face = Face.Ace
            let isDecrementBy1 () = 
                match foundation.Cards with
                | [] -> false
                | h::_ -> (int card.Face) + 1 = int h.Face

            let canAdd = 
                card.Suit = foundation.Suit &&
                ( isAceOverEmpty() || isDecrementBy1() )

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

        static member add (card: Card) foundations =
            match card.Suit with
            | Diamond -> { foundations with Diamond = Foundation.add card foundations.Diamond }
            | Club -> { foundations with Club = Foundation.add card foundations.Club }
            | Heart -> { foundations with Heart = Foundation.add card foundations.Heart }
            | Spade -> { foundations with Spade = Foundation.add card foundations.Spade }


    type Set = { 
        Tableau: Card list list; 
        Stock: Card list;
        Discard: Card list;
        Foundations: Foundations
    }