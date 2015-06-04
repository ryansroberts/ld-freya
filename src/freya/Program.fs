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

let fragment (Uri.Sys u) = u.Fragment
let removeHash (s : string) = s.Substring(1, (s.Length - 1))

let deltafile (prov : Provenance) =
  prov.InformedBy
  |> Seq.map (fragment >> removeHash)
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
  let map2 f g = PSeq.map (fun x -> (f x, g x))
  let prg = loadProvenance p
  let hasFailure = ref false
  let d = deltafile prg
  let kbg = Graph.empty (Uri.from "https://ld.nice.org.uk") domainSpaces

  let provFn = sprintf "%s/%s.prov.ttl" (string pth) d
  use provFile = toFile provFn :> System.IO.TextWriter
  use kbgFile = toFile (sprintf "%s/%s.ttl" (string pth) d) :> System.IO.TextWriter
  makeAll m prg.Targets
  |> PSeq.map (function
       | Failure(x, y) ->
         hasFailure := true
         (Failure(x, y))
       | x -> x)
  |> map2 pipeline.prov pipeline.extracted
  |> PSeq.iter (fun (prov, extracted) ->
       FSharp.RDF.Assertion.Assert.graph p prov |> ignore
       FSharp.RDF.Assertion.Assert.graph kbg extracted |> ignore)

  Graph.writeTtl provFile p

  printfn "#Wrote provenance to: %s" provFn
  match !hasFailure with
  | true ->
    printfn "#Build failed"
    1
  | false ->
    printfn "#Build successful"
    Graph.writeTtl kbgFile kbg
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
