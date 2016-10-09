// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Klondike
open Components
open System
open FSharp.Text.RegexProvider
open ConsolePlay

[<EntryPoint>]
let main argv = 
    Play.deal()
    |> play
    |> ignore

    Console.ReadKey() |> ignore
    0 // return an integer exit code
