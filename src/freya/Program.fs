open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Query
open FSharp.RDF
open Nessos.UnionArgParser
open Assertion
open Freya
open Freya.compilation
open Freya.Tools
open System.IO

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
let removeHash (s : string) = s.Substring(1, (s.Length - 1))

let deltafile prov =
  let c = fragment (prov.Commits.Head.Id) |> removeHash
  let c' = fragment (prov.Commits |> Seq.last).Id |> removeHash
  sprintf "%s-%s" c c'

let toFile (p) = new System.IO.StreamWriter(System.IO.File.OpenWrite p)
let (++) a b = System.IO.Path.Combine(a, b)

let hasFailure = Array.exists (function | (Failure _) -> true | _ -> false)

let domainSpaces = []

let compile pth m p d =
  let prg = (loadProvenance p)
  printfn "Tool configuration %A" m
  let xs =  makeAll m prg.Targets

  Assert.resources p ( xs |> Seq.map pipeline.prov |> List.ofSeq ) |> ignore

  graph.format graph.write.ttl (new StreamWriter(System.Console.OpenStandardOutput())) p |> ignore
  match hasFailure xs with
    | true -> exit 1
    | false ->
      let kbg = graph.empty (!"https://nice.org.uk") domainSpaces
      let rx = Seq.collect pipeline.extracted xs |> List.ofSeq
      Assert.resources kbg rx |> ignore

      let d = deltafile prg
      graph.format graph.write.ttl (graph.toFile (sprintf "%s/%s.prov.ttl" (string pth)d)) p |> ignore
      graph.format graph.write.ttl (graph.toFile (sprintf "%s/%s.ttl" (string pth) d)) kbg |> ignore
      exit 0

type Arguments =
  | Compilation of string
  | Provenence of string
  | Describe of string
  | Action of string
  | Output of string
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Compilation e -> "Path or url of compilation ontology"
      | Provenence t -> "Path or url to input provenance, if not specified prov is read from stdin"
      | Describe t -> "Display actions available at path"
      | Action t -> "Perform the specified action"
      | Output t -> "Directory to save compilaton output"

[<EntryPoint>]
let main argv =
  let parser = UnionArgParser.Create<Arguments>()
  let args = parser.Parse argv
  let makeOntology =
    graph.loadFrom (args.GetResult(<@ Compilation @>)) |> loadMake
  let toLower (s : string) = s.ToLower()
  let containsParam param = Seq.map toLower >> Seq.exists ((=) (toLower param))
  let paramIsHelp param =
    containsParam param [ "help"; "?"; "/?"; "-h"; "--help"; "/h"; "/help" ]
  if ((argv.Length = 2 && paramIsHelp argv.[1]) || argv.Length = 1) then
    printfn """Usage: freya [options]
                %s""" (parser.Usage())
    exit 1

  let prov = match args.TryGetResult <@ Provenence @> with
             | Some p -> p |> graph.loadFrom
             | _ -> graph.loadFormat (graph.parse.ttl) (System.IO.StreamReader(System.Console.OpenStandardInput()))
  compile (args.GetResult <@ Output @> |> toPath) makeOntology
    (prov)
    (args.GetResult <@ Output @>)
