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

//This is a ..remarkably inefficient way of recovering the hashes
let deltafile prov =
  let c = fragment ((prov.Commits |> Seq.head ) .Id) |> removeHash
  let c' = fragment (prov.Commits |> Seq.last).Id |> removeHash
  sprintf "%s-%s" c c'

let toFile (p) = new System.IO.StreamWriter(System.IO.File.OpenWrite p)
let (++) a b = System.IO.Path.Combine(a, b)

let hasFailure =
  Array.exists (function
    | (Failure _) -> true
    | _ -> false)

let domainSpaces = []

let compile pth m p d =
  let prg = (loadProvenance p)
  printfn "Tool configuration %A" m
  let xs = makeAll m prg.Targets
  Assert.triples p (xs
                    |> Seq.map pipeline.prov
                    |> List.ofSeq)
  |> Graph.streamTtl p (toStream (System.Console.OpenStandardOutput()))
  |> Seq.iter (fun _ -> ())
  match hasFailure xs with
  | true -> exit 1
  | false ->
    let kbg = Graph.empty (!"https://nice.org.uk") domainSpaces
    let rx = Seq.collect pipeline.extracted xs |> List.ofSeq
    Assert.graph kbg rx |> ignore
    let d = deltafile prg
    Graph.writeTtl
      (toFile (sprintf "%s/%s.prov.ttl" (string pth) d) :> System.IO.TextWriter)
      p
    Graph.writeTtl
      (toFile (sprintf "%s/%s.ttl" (string pth) d) :> System.IO.TextWriter) kbg
    exit 0

type Arguments =
  | Compilation of string
  | Provenance of string
  | Describe of string
  | Action of string
  | Output of string
  | Param of string * string
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Compilation _ -> "Path or url of compilation ontology"
      | Provenance _ ->
        "Path or url to input provenance, if not specified prov is read from stdin"
      | Describe _ -> "Display actions available at path"
      | Action _ -> "Perform the specified action"
      | Output _ -> "Directory to save compilaton output"
      | Param _ -> "Key value pair in the form key=value for action"

[<EntryPoint>]
let main argv =
  let parser = UnionArgParser.Create<Arguments>()
  let args = parser.Parse argv
  let makeOntology =
    Graph.loadFrom (args.GetResult(<@ Compilation @>)) |> loadMake
  let toLower (s : string) = s.ToLower()
  let containsParam param = Seq.map toLower >> Seq.exists ((=) (toLower param))
  let paramIsHelp param =
    containsParam param [ "help"; "?"; "/?"; "-h"; "--help"; "/h"; "/help" ]
  if ((argv.Length = 2 && paramIsHelp argv.[1]) || argv.Length = 1) then
    printfn """Usage: freya [options]
                %s""" (parser.Usage())
    exit 1
  let prov =
    match args.TryGetResult <@ Provenance @> with
    | Some p -> p |> Graph.loadFrom
    | _ -> Graph.loadTtl (fromStream (System.Console.OpenStandardInput()))
  compile (args.GetResult <@ Output @> |> toPath) makeOntology (prov)
    (args.GetResult <@ Output @>)
