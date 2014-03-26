module FsBowling

open Xunit
open FsUnit.Xunit

let calculate (input: string) =
    let rec calculate (input: char list) =
        let score c =
            if (c = '-') then
                0
            else
                System.Int32.Parse(string c)

        match input with
        | c :: '/' :: next :: [] -> 10 + score next
        | c :: '/' :: next :: rest -> 10 + score next + calculate (next :: rest)
        | 'X' :: next :: '/' :: rest -> 20 + calculate (next :: '/' :: rest)
        | 'X' :: next :: nextnext :: rest -> 10 + score next + score nextnext + calculate (next :: nextnext :: rest)
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
let ``Single throw at end game scores five`` ()=
    "------------------5-"
    |> calculate
    |> should equal 5

[<Fact>]
let ``Spare and gutter game scores ten`` ()=
    "4/------------------"
    |> calculate
    |> should equal 10

[<Fact>]
let ``Spare and following throw with gutter game scores sixteen`` ()=
    "4/3-----------------"
    |> calculate
    |> should equal 16

[<Fact>]
let ``Spare and following throw at end of gutter game scores thirteen`` ()=
    "-----------------4/3"
    |> calculate
    |> should equal 13

[<Fact>]
let ``Strike and gutter game scores ten`` ()=
    "X------------------"
    |> calculate
    |> should equal 10

[<Fact>]
let ``Strike and following throw with gutter game scores twenty`` ()=
    "X5-----------------"
    |> calculate
    |> should equal 20

[<Fact>]
let ``Strike and following two throws with gutter game scores twenty-six`` ()=
    "X53----------------"
    |> calculate
    |> should equal 26

[<Fact>]
let ``Strike and following spare with gutter game scores thirty`` ()=
    "X5/----------------"
    |> calculate
    |> should equal 30
