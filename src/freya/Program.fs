open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Query
open Nessos.UnionArgParser
open Freya
open Freya.compilation
open Freya.Tools
open System.IO
open ExtCore

open FSharp.RDF

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
let deltafile (prov:Provenance) =
  prov.InformedBy
  |> Seq.map ( fragment >> removeHash )
  |> Seq.fold (+) ""

let toFile (p) = new System.IO.StreamWriter(System.IO.File.OpenWrite p)
let (++) a b = System.IO.Path.Combine(a, b)

let hasFailure =
  Array.exists (function
    | (Failure _) -> true
    | _ -> false)

let domainSpaces = []


open FSharp.Collections.ParallelSeq
let compile pth m p d =
  let map2 f g = PSeq.map (fun x -> (f x,g x))
  let prg = loadProvenance p
  printfn "Tool configuration %A" m
  let hasFailure = ref false
  let d = deltafile prg
  let kbg = Graph.empty (Uri.from "https://nice.org.uk") domainSpaces

  let provStdio = Graph.streamTtl p (toStream(System.Console.OpenStandardOutput()))
  let provFile  = Graph.streamTtl p (toFile (sprintf "%s/%s.prov.ttl" (string pth) d) :> System.IO.TextWriter)

  makeAll m prg.Targets
  |> PSeq.map (function
                | Failure (x,y) -> hasFailure :=  true; (Failure (x,y))
                | x -> x)
  |> map2 pipeline.prov pipeline.extracted
  |> PSeq.iter (fun (prov,extracted) ->
                printfn "Iter result"
                FSharp.RDF.Assertion.Assert.triples p [prov]
                |> provStdio
                |> provFile
                |> ignore

                FSharp.RDF.Assertion.Assert.graph kbg extracted
                |> ignore
               )
  match !hasFailure with
  | true -> 1
  | false ->
    Graph.writeTtl (toFile (sprintf "%s/%s.prov.ttl" (string pth) d) :> System.IO.TextWriter) kbg
    0

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
  compile (args.GetResult <@ Output @> |> Path.from) makeOntology (prov)
    (args.GetResult <@ Output @>)
