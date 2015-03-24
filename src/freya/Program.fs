open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Query
open FSharp.RDF
open Model
open Nessos.UnionArgParser

type Arguments =
  | CompilationOntology of string
  | Provenance of string
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | CompilationOntology e -> "Path or url of compilation ontology"
      | Provenance p -> "Path or url to input provenance"




[<EntryPoint>]
let main argv =
  let parser = UnionArgParser.Create<Arguments>()
  let args = parser.Parse argv
  let ont = (args.GetResult(<@ CompilationOntology @>))
  let prov = (args.GetResult(<@ Provenance @>))
  let prov = Graph.from prov
  prov
  |> loadCompilation
  |> printfn "%A"
  0 // return an integer exit code
