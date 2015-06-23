#r "../../Packages/json-ld.net/lib/net40-Client/JsonLD.dll"
#r "../../Packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../Packages/dotNetRdf/lib/net40/dotNetRdf.dll"
#r "../../Packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../Packages/ExtCore/lib/net40/ExtCore.dll"
#I "../../Packages/FParsec/lib/net40-client"
#r "../../Packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../Packages/FParsec/lib/net40-client/FParsecCS.dll"
#I "../../Packages/FSharp.RDF/lib/net40"
#r "../../Packages/FSharp.RDF/lib/net40/owlapi.dll"
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

let rec take n xs =
  match n,xs with
  | _,[] -> []
  | 0,_ -> []
  | n,x::xs -> x::(take (n-1) xs)

open Freya
Path.from "application/pdf" ++ (FullName("lol","wut"))
