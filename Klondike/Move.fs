﻿namespace Klondike
module Move =    
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
                        then set.Tableau 
                            |> List.except [ pile ] 
                            |> (fun r -> (TableauPile xs) :: r)
                        else set.Tableau
                { From = from'; To = to' }
        { set with Tableau = transfer.From; Foundations = transfer.To }

    let fromDiscardToTableau set =
        ()

    let fromTableauToTableau pileIdx1 pileIdx2 set =
        ()