namespace Freya

module results =
  open FSharp.RDF
  open Assertion

  type Delta =
    { From : Uri
      To : Uri
      Path : Path }

  type OntologyVersion =
    { Version : Uri
      Path : Path }

  type CompilationOutput =
    { Path : Path
      Tip : OntologyVersion
      WorkingArea : Delta }

  let fragment (Uri.Sys u) = u.Fragment

  let deltafile prov =
    let c = fragment (prov.Commits.Head.Id)
    let c' = fragment (prov.Commits |> Seq.last).Id
    sprintf "{%s}-{%s}" c c'

  let saveCompilation (p : Path) (prov : Provenance) (pr : Graph)
      (r : ToolSuccess list) : unit =
    let kbg = graph.empty (!"http://nice.org.uk/") []
    for ({ Prov = px; Output = ox }) in r do
      Assert.resources kbg ox |> ignore
      Assert.resources pr px |> ignore
    let d = deltafile prov
    let kbn = (sprintf "%s/%s.ttl" (string p) d)
    let prn = (sprintf "%s/%s.prov.ttl" (string p) d)
    graph.format graph.write.ttl (graph.toFile kbn) kbg |> ignore
    graph.format graph.write.ttl (graph.toFile prn) pr |> ignore
