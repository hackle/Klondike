namespace Klondike
module Move =    
    open ListExtensions
    open Components
    type Transfer<'s, 'f> = { From: 's; To: 'f }
    
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

    let fromDiscardToTableau pileIdx set =
        let transfer =
            match set.Discard with
            | [] -> { From = set.Discard; To = set.Tableau }
            | x::xs ->
                let pile = set.Tableau.[pileIdx]
                let pile' = 
                    pile 
                    |> TableauPile.add x
                let to' = List.replacei pileIdx pile' set.Tableau
                let from' = 
                    if pile'.Value |> List.contains x
                        then xs
                        else set.Discard
                { From = from'; To = to' }

        { set with Discard = transfer.From; Tableau = transfer.To }

    let fromTableauToTableau pileIdx1 pileIdx2 set =
        let original = { From = set.Tableau.[pileIdx1]; To = set.Tableau.[pileIdx2] }
        let transfer =
            match original.From.Value with
            | [] -> original
            | x::xs ->
                let to' = original.To |> TableauPile.add x
                let from' =
                    if to'.Value |> List.contains x
                    then TableauPile xs
                    else original.From
                { From = from'; To = to' }

        let tableau' = 
            set.Tableau
            |> List.replacei pileIdx1 transfer.From
            |> List.replacei pileIdx2 transfer.To

        { set with Tableau = tableau' }