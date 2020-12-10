module Day7

open Expecto

type NodeId = string
type Nodes = Set<NodeId>
type Parents = Nodes
type Tree = Map<NodeId, Parents>

let rec getAncestors (tree: Tree) (node: NodeId) (acc: Nodes): Nodes =
    let parents = tree.Item(node)

    if parents.IsEmpty then
        acc
    else
        parents
        |> Seq.fold (fun acc n -> getAncestors tree n (Set.union acc parents)) acc

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
          } ]
