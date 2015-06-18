namespace Freya
open Elasticsearch.Net
open System
open FSharp.RDF
open FSharp.Data
open FSharp.RDF.JsonLD
open Newtonsoft.Json.Linq

module Elastic =


type Client =
  | Client of IElasticsearchClient


let rec toDoc r cl = ()
let addToElastic (id,d) cl = ()


let indexProv xr (Client cl) = ()


//Context selection will need to be subtler than this, we should be able to determin
//Which contexts we require from build prov
let context =
  """
    [
    "http://ld.nice.org.uk/ns/qualitystandard.jsonld",
    "http://ld.nice.org.uk/ns/compilation.jsonld"
    ]
  """
  |> Newtonsoft.Json.Linq.JObject.Parse

//We should be able to currently identify record subgraphs currently by following the prov -
//If each elastic record corresponds to statements extracted from a single source file,
//we just need to follow the prov for each file, when this is no longer a 1:1 correspondence,
//things are going to be trickier. This might be a related problem to deciding when to rebuild
//composite documents
let dodgyRecordHeuristic g =

  let traverse (p:string) = function | Traverse (Uri.from p) x -> x | _ -> []

  [for r in (Resource.fromType (Uri.from "prov:Entity") g) do
   yield (Seq.collect (traverse "prov:qualifiedDerivation") [r]
          |> Seq.collect (traverse "prov:entity")
          |> Seq.head,r)]
  |> Seq.groupBy fst

let append p e c =
  Graph.merge p e
  |> dodgyRecordHeuristic
  |> Seq.map snd
  |> Seq.map (Seq.map snd)
  |> Seq.map (Resource.toJsonLD ((JsonLD.Core.JsonLdOptions())))
