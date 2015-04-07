namespace Freya

open FSharp.RDF
open Assertion
open rdf
open owl
open compilation

module tools =
  let content (c : ToolMatch) =
    Success ([], [ owl.individual c.Target.Id [ c.Represents ]
                  [ dataProperty !"cnt:chars" (c.Target.Content ^^ xsd.string) ] ])

  let exec t =
    match t with
    | Content -> content

  let execMatches xtm =
    match xtm with
    | [ x ] ->
      [ for tl in x.Tools -> x |> exec tl ]
    | x :: xs -> [ Failure ([],[])  ]
    | [] -> []

  let make xrp t =
    let matched = xrp
                    |> List.map (toolsFor t)
                    |> List.choose id

    matched |> List.iter (fun tm -> printfn "Applying %A to %s" tm.Tools (string tm.Target.Path))
    execMatches matched

  let makeAll xrp xt = xt |> List.collect (make xrp)
