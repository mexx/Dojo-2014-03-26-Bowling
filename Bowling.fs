module FsBowling

open Xunit
open FsUnit.Xunit

let calculate input =
    let score c =
        if (c = '-') then
            0
        else
            System.Int32.Parse(string c)

    input
    |> Seq.map score
    |> Seq.sum

[<Fact>]
let ``Gutter game scores zero`` ()=
    "--------------------"
    |> calculate
    |> should equal 0

[<Fact>]
let ``Single throw game scores four`` ()=
    "4-------------------"
    |> calculate
    |> should equal 4
