namespace Freya

open FSharp.RDF
open FSharp.Markdown
open Assertion
open rdf
open owl
open Freya.Tracing
open Freya.YamlParser
open System
open Freya

module Tools =
  let either = function
    | ToolExecution.Failure x | ToolExecution.Success x -> x

  //Pass previously extracted resources from this pipeline into step
  let step f (PipelineStep(m, xp)) =
    let xr =
      xp
      |> List.map either
      |> List.collect (fun { Provenance = _; Extracted = r } -> r)

    let res = f m xr
    PipelineStep(m, res :: xp)

  open Pandoc

  let convertMarkdownS t m xr =
    match xr with
    | r :: xr ->
      let prov = convertResources r [] t
      pipeline.succeed prov [ r ]
    | [] -> pipeline.fail []

  let convertMarkdown x t =
    step (convertMarkdownS (x,
                            { Output = Path.from "/artifacts/work"
                              ToolMatch = t
                              WorkingDir = Path.from "." }))

  let exec tm =
    function
    | Pandoc m -> convertMarkdown m tm
    | SemanticExtractor(n, ToolFn f) -> (step f)

  //Produce a single PipelineStep -> PipelineStep from the tool list to apply to the source, this way they
  //will execute sequentially as part of a parallel sequence
  let execMatches x =
    (x.Tools
     |> List.map (exec x)
     |> List.reduce (>>)) (PipelineStep(x, []))

  let make xrp t =
    xrp
    |> List.map (Target.toolsFor t)
    |> List.choose id
    |> List.map execMatches

  open FSharp.Collections.ParallelSeq

  let makeAll xrp xt =
    PSeq.collect (make xrp) xt |> PSeq.map pipeline.result
