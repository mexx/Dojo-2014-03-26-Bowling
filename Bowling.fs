module FsBowling

open Xunit
open FsUnit.Xunit

let calculate input =
    let rec calculate input =
        let score = function
            | '-'
            | '/' -> 0
            | 'X' -> 10
            | c -> System.Int32.Parse(string c)

        let notAtEnd rest f =
            match rest with
            | [] -> 0
            | _ -> f

        match input with
        | _ :: '/' :: 'X' :: rest -> 20 + notAtEnd rest (calculate ('X' :: rest))
        | _ :: '/' :: next :: rest -> 10 + score next + notAtEnd rest (calculate (next :: rest))
        | 'X' :: next :: '/' :: rest -> 20 + notAtEnd rest (calculate (next :: '/' :: rest))
        | 'X' :: next :: nextnext :: rest -> 10 + score next + score nextnext + notAtEnd rest (calculate (next :: nextnext :: rest))
        | c :: rest -> score c + calculate rest
        | [] -> 0

    input
    |> List.ofSeq
    |> calculate

let convert lst =
    let parse x =
        let gutter x =
            match x with
            | 0 -> "-"
            | x -> string x

        match x with
        | (10, 0) -> "X"
        | (first, second) when first + second = 10 -> sprintf "%s/" (gutter first)
        | (first, second) -> sprintf "%s%s" (gutter first) (gutter second)

    let rec pairs lst =
        match lst with
        | a :: b :: rest -> (a, b) :: pairs rest
        | [] -> []

    lst
    |> pairs
    |> Seq.map parse
    |> Seq.collect id

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

[<Fact>]
let ``Strike and following strike with gutter game scores thirty`` ()=
    "XX----------------"
    |> calculate
    |> should equal 30

[<Fact>]
let ``Triple strike with gutter game scores sixty`` ()=
    "XXX---------------"
    |> calculate
    |> should equal 60

[<Fact>]
let ``Spare with following strike with gutter game scores thirty`` ()=
    "4/X-----------------"
    |> calculate
    |> should equal 30

[<Fact>]
let ``Spare with following strike at end of gutter game scores twenty`` ()=
    "-----------------4/X"
    |> calculate
    |> should equal 20

[<Fact>]
let ``Double strike at end of gutter game scores twenty`` ()=
    "-----------------XX-"
    |> calculate
    |> should equal 20

[<Fact>]
let ``Strike gutter strike at end of gutter game scores twenty`` ()=
    "-----------------X-X"
    |> calculate
    |> should equal 20

[<Fact>]
let ``Triple strike at end of gutter game scores thirty`` ()=
    "-----------------XXX"
    |> calculate
    |> should equal 30

[<Fact>]
let ``Perfect game scores three-hundred`` ()=
    "XXXXXXXXXXXX"
    |> calculate
    |> should equal 300

[<Fact>]
let ``Strike gutter spare at end of gutter game scores twenty`` ()=
    "-----------------X-/"
    |> calculate
    |> should equal 20

[<Fact>]
let ``Real game scores`` ()=
    "45332/31X9-63-/8/16"
    |> calculate
    |> should equal 105

[<Fact>]
let ``Real game scores with spare at end`` ()=
    "45332/31X9-63-/8/8/3"
    |> calculate
    |> should equal 118

[<Fact>]
let ``Real game scores with strike at end`` ()=
    "45332/31X9-63-/8/X23"
    |> calculate
    |> should equal 122


[<Fact>]
let ``Convert`` ()=
    [4;5;3;3;2;8;3;1;10;0;9;0;6;3;0;10;8;2;1;6]
    |> convert
    |> calculate
    |> should equal 105

