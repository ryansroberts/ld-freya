
open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Query
open FSharp.RDF
open Nessos.UnionArgParser
open Assertion
open Freya
open Freya.compilation
open Freya.tools

let empty() = Graph(new VDS.RDF.Graph())
let toFile (p) = new System.IO.StreamWriter(System.IO.File.OpenWrite p)
let (++) a b = System.IO.Path.Combine(a, b)
let fragment (Uri.Sys u) = u.Fragment

let compile pth m p d =
    let pr = (loadProvenance p)
    let xe = makeAll m pr.Targets

    let failure =
      function
      | Failure(_) -> true
      | _ -> false
    match List.partition failure xe with
    | x :: xs, _ ->
      [ for (Failure { Prov = px }) in x :: xs do
          yield! px ]
      |> Assert.resources p
      |> graph.format graph.write.ttl System.Console.Error
      |> ignore
      1
    | [], x :: xs ->
      [ for (Success s) in x :: xs -> s ]
      |> results.saveCompilation pth pr p
      |> ignore
      0

type Arguments =
    | [<Mandatory>] Compilation of string
    | Provenence of string
    | Describe of string
    | Action of string
    | Output of string
    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Compilation e -> "Path or url of compilation ontology"
        | Provenence t -> "Path or url to input provenance"
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
    let containsParam param =
      Seq.map toLower >> Seq.exists ((=) (toLower param))
    let paramIsHelp param =
      containsParam param [ "help"; "?"; "/?"; "-h"; "--help"; "/h"; "/help" ]
    if ((argv.Length = 2 && paramIsHelp argv.[1]) || argv.Length = 1) then
      printfn """Usage: freya [options]
                %s""" (parser.Usage())
      exit 1
    compile (args.GetResult <@ Output @> |> Path.from) makeOntology
      (args.GetResult <@ Provenence @> |> graph.loadFrom)
      (args.GetResult <@ Output @>)
