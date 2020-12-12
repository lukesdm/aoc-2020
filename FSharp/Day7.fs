module Day7

open Expecto
open System.Text.RegularExpressions

type NodeId = string
type Nodes = Set<NodeId>
type Parents = Nodes
type Dag = Map<NodeId, Parents>

type BagKind = string
type BagCount = BagKind * int

type Rule =
    { BagKind: BagKind
      Contains: list<BagCount> }

let rec getAncestors (dag: Dag) (node: NodeId) (acc: Nodes): Nodes =
    let parents = dag.Item(node)

    if parents.IsEmpty then
        acc
    else
        parents
        |> Seq.fold (fun acc n -> getAncestors dag n (Set.union acc parents)) acc

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

let parseRules (input: string []): seq<Rule> = input |> Seq.map parse

let getParents (dag: Dag) (node: NodeId): Parents =
    match dag.TryFind node with
    | Some (parents) -> parents
    | None -> Set.empty

let addParent (dag: Dag) (node: NodeId) (parent: NodeId): Dag =
    let parents = (getParents dag node).Add(parent)
    dag.Add(node, parents)

/// Builds dag from given rules, storing nodes and their parents in a hashmap, referenced by bag colour
let buildDag (dag: Dag) (rule: Rule): Dag =
    let dag =
        if dag.ContainsKey(rule.BagKind) then dag else dag.Add(rule.BagKind, Set.empty)

    rule.Contains
    |> Seq.fold (fun acc (node, _) -> addParent acc node rule.BagKind) dag

let parseIntoDag (input: string []): Dag =
    input |> parseRules |> Seq.fold buildDag Map.empty

let part1 (input: string []): int =
    getAncestors (parseIntoDag input) "shiny gold" Set.empty
    |> Set.count

/// ***PART 2***

// NodeId and bag count
type Nodes2 = Set<NodeId * int>
type Children = Nodes2
type Dag2 = Map<NodeId, Children>

let createNode2 (rule: Rule): (NodeId * Children) =
    (rule.BagKind, Set.ofList rule.Contains)

let buildDag2 (dag: Dag2) (rule: Rule): Dag2 = dag.Add(createNode2 rule)

let parseIntoDag2 (input: string []): Dag2 =
    input
    |> parseRules
    |> Seq.fold buildDag2 Map.empty

let rec getChildren (dag: Dag2) (node: NodeId) (acc: Nodes2): Nodes2 =
    let children = dag.Item(node)

    if children.IsEmpty then
        acc
    else
        children
        |> Seq.fold (fun acc (n, _) -> getChildren dag n (Set.union acc children)) acc

let rec countDescendants (dag: Dag2) (node: NodeId): int =
    let children = dag.Item(node)

    if children.IsEmpty then
        0
    else
        (children
         |> Seq.map (fun (child_id, child_count) -> child_count * (1 + countDescendants dag child_id))
         |> Seq.sum)

let part2 (input: string []): int =
    countDescendants (parseIntoDag2 input) "shiny gold"

// ***...***

let solve input = (part1 input, part2 input)

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
            let dag: Dag =
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


            let ancestors_actual = getAncestors dag "G" Set.empty

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

              let rules_actual = parseRules input

              Expect.equal (List.ofSeq rules_actual) rules_expected ""
          }
          test "Part 1 - Example 1 - Build DAG from rules" {
              let input =
                  example1.Replace("\r\n", "\n").Split("\n")

              let expected: Dag =
                  Map.ofList [ ("bright white",
                                Set.ofList [ "dark orange"
                                             "light red" ])
                               ("dark olive", Set.ofList [ "shiny gold" ])
                               ("dark orange", Set.empty)
                               ("dotted black",
                                Set.ofList [ "dark olive"
                                             "vibrant plum" ])
                               ("faded blue",
                                Set.ofList [ "dark olive"
                                             "vibrant plum"
                                             "muted yellow" ])
                               ("light red", Set.empty)
                               ("muted yellow",
                                Set.ofList [ "dark orange"
                                             "light red" ])
                               ("shiny gold",
                                Set.ofList [ "bright white"
                                             "muted yellow" ])
                               ("vibrant plum", Set.ofList [ "shiny gold" ]) ]

              let actual = parseIntoDag input

              Expect.equal actual expected ""
          }
          test "Part 1 - Example 1 - Count bag ancestors" {
              let inputDag =
                  example1.Replace("\r\n", "\n").Split("\n")
                  |> parseIntoDag

              // "in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4"
              let bagKinds_expected = 4

              let bagKinds_actual =
                  getAncestors inputDag "shiny gold" Set.empty

              Expect.equal bagKinds_actual.Count bagKinds_expected ""
          }
          test "Part 2 - Example 1 - Build DAG" {
              let input =
                  example1.Replace("\r\n", "\n").Split("\n")

              let expected: Dag2 =
                  Map.ofList [ ("light red",
                                Set.ofList [ ("bright white", 1)
                                             ("muted yellow", 2) ])
                               ("dark orange",
                                Set.ofList [ ("bright white", 3)
                                             ("muted yellow", 4) ])
                               ("bright white", Set.ofList [ ("shiny gold", 1) ])
                               ("muted yellow",
                                Set.ofList [ ("shiny gold", 2)
                                             ("faded blue", 9) ])
                               ("shiny gold",
                                Set.ofList [ ("dark olive", 1)
                                             ("vibrant plum", 2) ])
                               ("dark olive",
                                Set.ofList [ ("faded blue", 3)
                                             ("dotted black", 4) ])
                               ("vibrant plum",
                                Set.ofList [ ("faded blue", 5)
                                             ("dotted black", 6) ])
                               ("faded blue", Set.empty)
                               ("dotted black", Set.empty) ]

              let actual = parseIntoDag2 input

              Expect.equal actual expected ""
          }
          test "Part 2 - Example 1 - Total contained bags" {
              let input =
                  example1.Replace("\r\n", "\n").Split("\n")
              // So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it)
              // plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags
              Expect.equal (part2 input) 32 ""
          } ]
