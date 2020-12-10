module Day7

open Expecto
open System.Text.RegularExpressions

type NodeId = string
type Nodes = Set<NodeId>
type Parents = Nodes
type Tree = Map<NodeId, Parents>

type BagKind = string
type BagCount = BagKind * int

type Rule =
    { BagKind: BagKind
      Contains: list<BagCount> }

let rec getAncestors (tree: Tree) (node: NodeId) (acc: Nodes): Nodes =
    let parents = tree.Item(node)

    if parents.IsEmpty then
        acc
    else
        parents
        |> Seq.fold (fun acc n -> getAncestors tree n (Set.union acc parents)) acc

/// Given "striped maroon bags contain 4 vibrant tomato bags, 2 dull salmon bags, 1 shiny gold bag, 2 muted coral bags."
/// Matches will be 0:"striped maroon" 1:"4 vibrant tomato" 2:"2 dull salmon" 3:"1 shiny gold" 4:"2 muted coral"
/// Given "dim olive bags contain no other bags."
/// Matches will be 0:"dim olive" 1:"no other"
/// Group 1 of each match is the useful part, but needs further parsing to extract any number
let regex = new Regex("((?:\d )?\w+ \w+) bag")

// input e.g. "4 vibrant tomato" (always single digit counts)
let parseBagCount (input: string): BagCount = (input.[2..], int input.[0..0])

let parse (input: string): Rule =
    let matches = regex.Matches(input)
    let bagKind = matches.[0].Groups.[1].Value

    let contains: seq<BagCount> =
        if matches.[1].Value.StartsWith("no") then
            Seq.empty
        else
            Seq.skip 1 matches
            |> Seq.map (fun (m: Match) -> parseBagCount m.Groups.[1].Value)

    { BagKind = bagKind
      Contains = List.ofSeq contains }

let parseAll (input: string []): seq<Rule> = input |> Seq.map parse

let example1 = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."

let tests =
    testList
        "Day 7"
        [ test "Get ancestors" {
            let tree: Tree =
                Map.ofList [ ("A", Set.empty)
                             ("B", Set.empty)
                             ("C", Set.empty)
                             ("D", Set.ofList [ "A"; "B" ])
                             ("E", Set.empty)
                             ("F", Set.ofList [ "C" ])
                             ("G", Set.ofList [ "D"; "E"; "F" ]) ]

            let ancestors_expected =
                Set.ofList [ "D"
                             "E"
                             "F"
                             "A"
                             "B"
                             "C" ]


            let ancestors_actual = getAncestors tree "G" Set.empty

            Expect.equal ancestors_actual ancestors_expected ""
          }
          test "Example 1 - Can parse rules" {
              let input =
                  example1.Replace("\r\n", "\n").Split("\n")

              let rules_expected =
                  [ { BagKind = "light red"
                      Contains =
                          [ ("bright white", 1)
                            ("muted yellow", 2) ] }
                    { BagKind = "dark orange"
                      Contains =
                          [ ("bright white", 3)
                            ("muted yellow", 4) ] }
                    { BagKind = "bright white"
                      Contains = [ ("shiny gold", 1) ] }
                    { BagKind = "muted yellow"
                      Contains = [ ("shiny gold", 2); ("faded blue", 9) ] }
                    { BagKind = "shiny gold"
                      Contains =
                          [ ("dark olive", 1)
                            ("vibrant plum", 2) ] }
                    { BagKind = "dark olive"
                      Contains =
                          [ ("faded blue", 3)
                            ("dotted black", 4) ] }
                    { BagKind = "vibrant plum"
                      Contains =
                          [ ("faded blue", 5)
                            ("dotted black", 6) ] }
                    { BagKind = "faded blue"
                      Contains = [] }
                    { BagKind = "dotted black"
                      Contains = [] } ]

              let rules_actual = parseAll input

              Expect.equal (List.ofSeq rules_actual) rules_expected ""
          } ]
