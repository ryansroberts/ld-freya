module Tools

open Model
open FSharp.RDF
open Assertion

let echoContent (c : Target) g = 
  let triples = triples g
  let puri = puri g
  let qn = qn g
  let literal = literal g
  let a = a g
  ()
//triples (puri c.Id,
//         [ (a, qn "cnt:ContentAsText")
//           (qn "cnt:chars", literal c.Content) ])
