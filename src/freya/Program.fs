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

//this is shit
let saveCompilation p prov pr (t, xt) : unit =
  let kbg = graph.empty (!"http://nice.org.uk/") []
  let toResources id = rdf.resource id
  let { Provenence = provStatements; Extracted = extracted } = xt
  Assert.resources kbg extracted |> ignore
  Assert.resources pr [ toResources t.ProvId provStatements ] |> ignore
  let d = deltafile prov
  let kbn = (sprintf "%s/%s.ttl" (string p) d)
  let prn = (sprintf "%s/%s.prov.ttl" (string p) d)
  printfn "Writing compilation to %s and prov to %s" kbn prn
  graph.format graph.write.ttl (graph.toFile kbn) kbg |> ignore
  graph.format graph.write.ttl (graph.toFile prn) pr |> ignore
  graph.format graph.write.ttl (StreamWriter(System.Console.OpenStandardOutput())) pr |> ignore

let toFile (p) = new System.IO.StreamWriter(System.IO.File.OpenWrite p)
let (++) a b = System.IO.Path.Combine(a, b)

let compile pth m p d =
  let pr = (loadProvenance p)
  printfn "Tool configuration %A" m
  match makeAll m pr.Targets with
  | PipelineExecution.Success(t, xt) ->
    saveCompilation pth pr p (t, xt)
    exit 0
  | PipelineExecution.Failure(t, { Provenence = prov; Extracted = _ }) ->
    Assert.resources p [ rdf.resource t.Id prov ]
    |> graph.format graph.write.ttl System.Console.Error
    |> ignore
    exit 1

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
