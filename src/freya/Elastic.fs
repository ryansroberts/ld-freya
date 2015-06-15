namespace Freya
open Elasticsearch.Net
open System
open FSharp.RDF
open FSharp.Data
open Newtonsoft.Json.Linq
module Elastic =


type Client =
  | Client of IElasticsearchClient


let rec toDoc r cl = ()
let addToElastic (id,d) cl = ()


let indexProv xr (Client cl) = ()

