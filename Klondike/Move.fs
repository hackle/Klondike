namespace Klondike
module Move =    
    open ListExtensions
    open Components
    
    let fromStockToDiscard set =
        let result =
            match set.Stock with
            | [] -> { From = set.Discard; To = [] }
            | x::xs -> { From = xs; To = x :: set.Discard }

        { set with Stock = result.From; Discard = result.To }

    let fromDiscardToFoundation set =
        let transfer =
            match set.Discard with
            | [] -> { From = set.Discard; To = set.Foundations }
            | x::xs ->
                let to' = set.Foundations |> Foundations.add x
                let from' = 
                    if to' |> Foundations.has x 
                        then xs
                        else set.Discard
                { From = from'; To = to' }

        { set with Discard = transfer.From; Foundations = transfer.To }

    let fromTableauToFoundation pileIdx set =
        let pile = set.Tableau.[ pileIdx ]
        let transfer =
            match pile.Value with
            | [] -> { From = set.Tableau; To = set.Foundations }
            | x::xs ->
                let to' = set.Foundations |> Foundations.add x
                let from' = 
                    if to' |> Foundations.has x 
                        then set.Tableau |> List.replacei pileIdx (TableauPile xs)
                        else set.Tableau
                { From = from'; To = to' }
        { set with Tableau = transfer.From; Foundations = transfer.To }
    
    let canAddTo' (dest: TableauPile) (card: Card) =
        let currentMaxValue = 
            if dest.Value.IsEmpty 
            then
                EnumHelper.allValues<Face>()
                |> Seq.max
                |> int
                |> (+) 1
            else dest.Value.Head.Face |> int
        let valueMatches = int card.Face = currentMaxValue - 1

        let colorMatches =
            if dest.Value.IsEmpty
            then true
            else
                card |> Card.color =
                    match dest.Value.Head |> Card.color with
                    | Red -> Black
                    | Black -> Red
            
        valueMatches && colorMatches
    
    let addOrderedCardsToPile ordered pile =
        let canJoinAt = 
            ordered 
            |> List.tryFindIndex (canAddTo' pile)

        match canJoinAt with
        | None -> { From = ordered; To = pile }
        | Some idx -> 
            let (suitable, unsuitable) = ordered |> List.splitAt (idx + 1)
            { 
                From = unsuitable; 
                To = suitable @ pile.Value |> TableauPile 
            }

    let joinPiles pile1 pile2 =
        let ordered = pile1 |> TableauPile.ordered
        let transfer = addOrderedCardsToPile ordered pile2
        if transfer.From = ordered
        then { From = pile1; To = pile2 }
        else 
            { 
                From = TableauPile (pile1.Value |> List.except ordered);
                To = transfer.To
            }    

    let fromDiscardToTableau pileIdx set =
        match set.Discard with
        | [] -> set
        | x::xs ->
            let transfer = addOrderedCardsToPile [ x ] set.Tableau.[ pileIdx ]
            { set with 
                Discard = if transfer.From.IsEmpty then xs else set.Discard; 
                Tableau = set.Tableau |> List.replacei pileIdx transfer.To
            }    

    let fromTableauToTableau pileIdx1 pileIdx2 set =
        let original = { From = set.Tableau.[pileIdx1]; To = set.Tableau.[pileIdx2] }
        let transfer = joinPiles original.From original.To

        let tableau' = 
            set.Tableau
            |> List.replacei pileIdx1 transfer.From
            |> List.replacei pileIdx2 transfer.To

        { set with Tableau = tableau' }