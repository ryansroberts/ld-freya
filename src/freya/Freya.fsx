#r "../../Packages/dotNetRdf/lib/net40/dotNetRdf.dll"
#r "../../Packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../Packages/ExtCore/lib/net40/ExtCore.dll"
#r "../../Packages/FSharp.RDF/lib/net40/FSharp.RDF.dll"
#r "../../Packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../../Packages/FSharp.Formatting/lib/net40/FSharp.Markdown.dll"
#r "../../Packages/SharpYaml/lib/SharpYaml.dll"
#load "Model.fs"
#load "Commands.fs"

open FSharp.RDF
open Freya.Commands

let qsCompilation = """
@prefix : <http://ld.nice.org.uk/ns/compilation#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://ld.nice.org.uk/ns/compilation/> .

:QualityStandards
  rdf:type :DirectoryPattern ,
  owl:NamedIndividual ;
  :expression "qualitystandards"^^xsd:string ;
  :parent :Root .

:QualityStatement
  rdf:type :FilePattern,
  owl:NamedIndividual ;
  :expression "statement_(?<QualityStatementId>.*).md"^^xsd:string ;
  :tool :Content ;
  :tool :YamlMetadata ;
  :represents :QualityStatement;
  :parent :QualityStandard .

:QualityStandard
  rdf:type :DirectoryPattern ,
           owl:NamedIndividual ;
  :expression "standard_(?<QualityStandardId>.*)"^^xsd:string ;
  :parent :QualityStandards .

"""

let from s = 
  String.split [| '/' |] s
  |> Array.map Freya.Segment
  |> List.ofArray
  |> Freya.Path

let g = graph.loadFormat graph.parse.ttl (graph.fromString qsCompilation)
let rp = Freya.compilation.loadMake g |> List.head
let desc = describe [ rp ] (from "qualitystandards/*")
