module RDF

open Model
open VDS.RDF

let literal (g : IGraph) s = g.CreateLiteralNode s :> INode
let uri (g : IGraph) u = UriFactory.Create u |> g.CreateUriNode :> INode
let puri (g : IGraph) (u : Model.Uri) = g.CreateUriNode(string u) :> INode
let qn (g : IGraph) (qn : string) = g.CreateUriNode qn :> INode
let a (g : IGraph) = qn g "rdf:type"
let date (g : IGraph) (d : System.DateTimeOffset) = 
  LiteralExtensions.ToLiteral(d, g) :> INode
let triples (g : IGraph) = function 
  | (s, px) -> 
    px
    |> List.map (function 
         | (p, o) -> Triple(s, p, o))
    |> g.Assert
    |> ignore
    ()

let blank (g : IGraph) px = 
  let b = g.CreateBlankNode()
  triples g (b, px) |> ignore
  b :> INode
