namespace Klondike

[<RequireQualifiedAccess>]
module Play =
    
    open Game
    open ListExtensions

    let deal () = 
        let allCardsShuffled = AllCards |> List.ofSeq |> List.shuffle
        let folder carry count =
            let (piles, rest) = carry
            let (take, leave) = List.splitAt count rest
            take :: piles, leave

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

