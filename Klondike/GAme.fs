namespace Klondike

module Game =
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

    type Card = { Suit: Suit; Face: Face }

    let AllCards =
        seq {
            for s in UnionCaseHelper.allUnionCases<Suit>() do
            for n in EnumHelper.allValues<Face>() do
            yield { Suit = s; Face = n }
        }

    type Foundation = 
        { 
            Suit: Suit; 
            Cards: Card list 
        }
            
    type Foundations = 
        {
            Diamond: Foundation;
            Club: Foundation;
            Heart: Foundation;
            Spade: Foundation;
        }

    type TableauPile = TableauPile of Card list
    with 
        member this.Value = 
            let (TableauPile cards) = this
            cards

    type Set = { 
        Tableau: TableauPile list;
        Stock: Card list;
        Discard: Card list;
        Foundations: Foundations
    }