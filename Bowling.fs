module FsBowling

open Xunit
open FsUnit.Xunit

let calculate (input: string) =
    let rec calculate (input: char list) =
        let score c =
            if (c = '-') then
                0
            elif (c = '/') then
                0
            else
                System.Int32.Parse(string c)

        match input with
        | c :: '/' :: rest -> 10 + calculate rest
        | c :: rest -> score c + calculate rest
        | [] -> 0

    input
    |> List.ofSeq
    |> calculate

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

[<Fact>]
let ``Single throw at end game scores four`` ()=
    "------------------5-"
    |> calculate
    |> should equal 5
    
[<Fact>]
let ``Spare and gutter game scores four`` ()=
    "4/------------------"
    |> calculate
    |> should equal 10
