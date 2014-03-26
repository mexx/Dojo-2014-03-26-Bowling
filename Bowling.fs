module FsBowling

open Xunit
open FsUnit.Xunit

let calculate input =
    0

[<Fact>]
let ``Gutter game scores zero`` ()=
    "--------------------"
    |> calculate
    |> should equal 0
