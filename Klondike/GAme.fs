namespace Klondike

module Game =
    open Microsoft.FSharp.Reflection

    let allUnionCases<'a>() = 
        FSharpType.GetUnionCases typeof<'a>
        |> Array.map (fun u -> FSharpValue.MakeUnion(u, [||]) :?> 'a)

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

    type Foundation = { Suit: Suit; Cards: Card list } with
        member this.Add (card: Card) = 
            let isAceOverEmpty () =
                this.Cards = [] && card.Face = Face.Ace
            let isDecrementBy1 () = 
                match this.Cards with
                | [] -> false
                | h::_ -> (int card.Face) + 1 = int h.Face

            let canAdd = 
                card.Suit = this.Suit &&
                ( isAceOverEmpty() || isDecrementBy1() )

            match canAdd with
            | true -> { this with Cards = card :: this.Cards }
            | false -> this
            
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

        member this.Add (card: Card) =
            match card.Suit with
            | Diamond -> { this with Diamond = card |> this.Diamond.Add }
            | Club -> { this with Club = card |> this.Club.Add }
            | Heart -> { this with Heart = card |> this.Heart.Add }
            | Spade -> { this with Spade = card |> this.Spade.Add }

    type Set = { 
        Tableau: Card list list; 
        Stock: Card list;
        Discard: Card list;
        Foundations: Foundations
    } with
        member this.addToFoundation card = 
            { this with Foundations = this.Foundations.Add card }

    let getAllCards () =
        let allSuits = allUnionCases<Suit>()
        let allFaces =
            [ (int Face.Two) .. (int Face.Ace) ]
            |> List.map (fun i -> enum i)

        seq {
            for s in allSuits do
            for n in allFaces do
            yield { Suit = s; Face = n }
        }

    let permute list =
        let rand = new System.Random()
        let rec permute' remaining result =
            match remaining with
            | [] -> result
            | _ ->
                let index = rand.Next(0, (List.length remaining))
                let item = remaining |> List.item index
                permute' (remaining |> List.except [ item ]) (item::result)
        permute' list []
        
    let takeFromList count list = 
        List.splitAt count

    let deal () = 
        let allCards = getAllCards() |> List.ofSeq |> permute
        let folder carry count =
            let (piles, rest) = carry
            let (take, leave) = List.splitAt count rest
            take :: piles, leave

        let (tableau, undealt) =
            [ 7 .. -1 .. 1 ]
            |> List.fold folder ([], allCards)

        {
            Tableau = tableau;
            Stock = undealt;
            Discard = [];
            Foundations = Foundations.New()
        }

    let transfer set = 
        let nextStock = if set.Stock = [] then set.Discard else set.Stock
        let nextDiscard = if set.Stock = [] then [] else set.Discard
        { set with Discard = nextDiscard; Stock = nextStock }

    let pickFromStock set =
        { 
            set with 
                Discard = (List.head set.Stock)::set.Discard;
                Stock = (List.tail set.Stock) 
        }

module DealerTests =
    open Xunit
    open Game

    [<Fact>]
    let ``Each set should have 7 piles``() =
        let set = deal()
        Assert.Equal(set.Tableau|>List.length, 7)

    [<Fact>]
    let ``First pile has 1 card, 2nd 2 cards and so on`` () =
        let set = deal()
        let assertItemAt idx = 
            let cards = set.Tableau |>List.item (idx - 1)
            Assert.Equal(idx, cards |> List.length)
        [ 1 .. 7 ]
        |> List.iter assertItemAt

    [<Fact>]
    let ``If the Stock becomes empty, turn the entire discard pile over and make it the new Stock.`` () =
        let stock = getAllCards() |> List.ofSeq |> List.take 5
        let next = 
            {
                Tableau = [];
                Stock = [];
                Discard = stock;
                Foundations = Foundations.New()
            }
            |> transfer

        Assert.Equal<Card list>(next.Stock, stock)

    [<Fact>]
    let ``Turn over the top card of the Stock and place it face-up on the Discard pile`` () =
        let (stock, discard) = 
            getAllCards() 
            |> List.ofSeq 
            |> List.take 10
            |> List.splitAt 5
        let next = 
            {
                Tableau = [];
                Stock = stock;
                Discard = discard;
                Foundations = Foundations.New()
            }
            |> pickFromStock

        Assert.Equal<Card list>(next.Stock, stock |> List.tail)
        Assert.Equal<Card list>(next.Discard, (List.head stock) :: discard)

    [<Fact>]
    let ``Move to foundation, if foundation is empty then only Ace is allowed`` () =
        let card = { Suit = Spade; Face = Face.Ace }
        let next = 
            {
                Tableau = [];
                Stock = [];
                Discard = [];
                Foundations = Foundations.New()
            }.addToFoundation card

        let actual = next.Foundations.Spade.Cards |> List.head

        Assert.Equal<Card>(card, actual)

    [<Fact>]
    let ``Move to foundation, if foundation is empty then none-Ace is not allowed`` () =    
        let card = { Suit = Spade; Face = Face.Two }
        let next = 
            {
                Tableau = [];
                Stock = [];
                Discard = [];
                Foundations = Foundations.New()
            }.addToFoundation card

        let actual = next.Foundations.Spade.Cards

        Assert.Equal<Card>([], actual)
