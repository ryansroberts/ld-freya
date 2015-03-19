#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharp.RDF/lib/net40/FSharp.RDF.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#load "Model.fs"

open FSharp.RDF
open System.IO
open Swensen.Unquote
open Model
open System.Text.RegularExpressions
open resource

let qsCompilation = """
@prefix : <http://nice.org.uk/ns/compilation#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://nice.org.uk/ns/compilation/> .

<http://nice.org.uk/ns/compilation/> rdf:type owl:Ontology ;

                                     owl:imports <http://nice.org.uk/ns/compilation> .



<http://nice.org.uk/ns/compilation#QualityStandards> rdf:type <http://nice.org.uk/ns/compilation#DirectoryPattern> ,
                                                              owl:NamedIndividual ;

                                                     <http://nice.org.uk/ns/compilation#expression> "qualitystandards"^^xsd:string ;

                                                     <http://nice.org.uk/ns/compilation#parent> <http://nice.org.uk/ns/compilation#Root> .


:QualityStatement rdf:type <http://nice.org.uk/ns/compilation#FilePattern> ,  owl:NamedIndividual ;
                  <http://nice.org.uk/ns/compilation#expression> "statement-(?<QualityStatementId>.*)"^^xsd:string ;
                  <http://nice.org.uk/ns/compilation#tool> <http://nice.org.uk/ns/compilation#Content> ;
                  <http://nice.org.uk/ns/compilation#represents> <http://nice.org.uk/ns/qualitystandard#QualityStatement>;
                  <http://nice.org.uk/ns/compilation#parent> <http://nice.org.uk/ns/compilation#QualityStandard> .


<http://nice.org.uk/ns/compilation#QualityStandard> rdf:type <http://nice.org.uk/ns/compilation#DirectoryPattern> ,
                                                              owl:NamedIndividual ;

                                                     <http://nice.org.uk/ns/compilation#expression> "qualitystandard_(?<QualityStandardId>.*)"^^xsd:string ;


                  <http://nice.org.uk/ns/compilation#parent> <http://nice.org.uk/ns/compilation#QualityStandards> .

    """
let g = Graph.from qsCompilation

test <@ loadMake g = List.Empty @>
