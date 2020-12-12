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

let parseRules (input: string []): seq<Rule> = input |> Seq.map parse

let getParents (tree: Tree) (node: NodeId): Parents =
    match tree.TryFind node with
    | Some (parents) -> parents
    | None -> Set.empty

let addParent (tree: Tree) (node: NodeId) (parent: NodeId): Tree =
    let parents = (getParents tree node).Add(parent)
    tree.Add(node, parents)

/// Builds tree from given rules, storing nodes and their parents in a hashmap, referenced by bag colour
let buildTree (tree: Tree) (rule: Rule): Tree =
    let tree =
        if tree.ContainsKey(rule.BagKind) then tree else tree.Add(rule.BagKind, Set.empty)

    rule.Contains
    |> Seq.fold (fun acc (node, _) -> addParent acc node rule.BagKind) tree

let parseIntoTree (input: string []): Tree =
    input
    |> parseRules
    |> Seq.fold buildTree Map.empty

let part1 (input: string []): int =
    getAncestors (parseIntoTree input) "shiny gold" Set.empty
    |> Set.count

/// ***PART 2***

// NodeId and bag count
type Nodes2 = Set<NodeId * int>
type Children = Nodes2
type Tree2 = Map<NodeId, Children>

let createNode2 (rule: Rule): (NodeId * Children) =
    (rule.BagKind, Set.ofList rule.Contains)

let buildTree2 (tree: Tree2) (rule: Rule): Tree2 = tree.Add(createNode2 rule)

let parseIntoTree2 (input: string []): Tree2 =
    input
    |> parseRules
    |> Seq.fold buildTree2 Map.empty

let rec getChildren (tree: Tree2) (node: NodeId) (acc: Nodes2): Nodes2 =
    let children = tree.Item(node)

    if children.IsEmpty then
        acc
    else
        children
        |> Seq.fold (fun acc (n, _) -> getChildren tree n (Set.union acc children)) acc

let rec countDescendants (tree: Tree2) (node: NodeId): int =
    let children = tree.Item(node)

    if children.IsEmpty then
        0
    else
        (children
         |> Seq.map (fun (child_id, child_count) -> child_count * (1 + countDescendants tree child_id))
         |> Seq.sum)

let part2 (input: string []): int =
    countDescendants (parseIntoTree2 input) "shiny gold"

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

              let rules_actual = parseRules input

              Expect.equal (List.ofSeq rules_actual) rules_expected ""
          }
          test "Part 1 - Example 1 - Build tree" {
              let input =
                  example1.Replace("\r\n", "\n").Split("\n")

              let expected: Tree =
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

              let actual = parseIntoTree input

              Expect.equal actual expected ""
          }
          test "Part 1 - Example 1 - Solve bag ancestors" {
              let inputTree =
                  example1.Replace("\r\n", "\n").Split("\n")
                  |> parseIntoTree

              // "in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4"
              let bagKinds_expected = 4

              let bagKinds_actual =
                  getAncestors inputTree "shiny gold" Set.empty

              Expect.equal bagKinds_actual.Count bagKinds_expected ""
          }
          test "Part 2 - Example 1 - Build Tree" {
              let input =
                  example1.Replace("\r\n", "\n").Split("\n")

              let expected: Tree2 =
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

              let actual = parseIntoTree2 input

              Expect.equal actual expected ""
          }
          test "Part 2 - Example 1 - Total bags" {
              let input =
                  example1.Replace("\r\n", "\n").Split("\n")
              // So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it)
              // plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags
              Expect.equal (part2 input) 32 ""
          } ]
