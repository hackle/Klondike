namespace Klondike
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
        ()

    let fromDiscardToTableau set =
        ()

    let fromTableauToFoundation pileIdx set =
        ()

    let fromTableauToTableau pileIdx1 pileIdx2 set =
        ()