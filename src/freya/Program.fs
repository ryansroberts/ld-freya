open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Query
open FSharp.RDF
open Model
open Nessos.UnionArgParser
open Assertion
open Tools

let empty () = Graph ( new VDS.RDF.Graph () )

let toFile (p) = new System.IO.StreamWriter ( System.IO.File.OpenWrite p  )

let (++) a b = System.IO.Path.Combine (a,b)
let fragment (Uri.Sys u) = u.Fragment


let compile m p d =
  let p = (loadProvenance p)
  let fn = (d ++ (fragment p.Id) + ".ttl")
  let xe = makeAll m p.Targets
  match failures xe with
    | [] ->
      [for (Success({Prov=_;Output=o})) in xe do yield! o]
      |> output.toGraph !"http://nice.org.uk/ns/compilation" []
      |> graph.format graph.write.ttl (toFile fn)
      |> ignore
      0
    | xf::[] -> 1

type Arguments =
  | [<Mandatory>]MakeOntology of string
  | Compile of string
  | Describe of string
  | Action of string
  | Output of string
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | MakeOntology e -> "Path or url of compilation ontology"
      | Compile t -> "Path or url to input provenance"
      | Describe t -> "Display actions available at path"
      | Action t -> "Perform the specified action"
      | Output t -> "Directory to save compilaton output"

[<EntryPoint>]
let main argv =
  let parser = UnionArgParser.Create<Arguments>()
  let args = parser.Parse argv
  let makeOntology = graph.loadFrom (args.GetResult(<@ MakeOntology @>)) |> loadMake

  let toLower (s:string) = s.ToLower()
  let containsParam param = Seq.map toLower >> Seq.exists ((=) (toLower param))
  let paramIsHelp param = containsParam param ["help"; "?"; "/?"; "-h"; "--help"; "/h"; "/help"]

  if (( argv.Length = 2 && paramIsHelp argv.[1] ) || argv.Length = 1) then
    printfn """Usage: freya [options]
                %s""" ( parser.Usage ()  )
    exit 1

  compile makeOntology (args.GetResult <@ Compile @> |> graph.loadFrom) (args.GetResult <@ Output @>)

