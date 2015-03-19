module freya.Tests

open Model
open System.Text.RegularExpressions
open FSharp.RDF
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Loading resource path from compilation ontology``() =
  let matchingTarget =
    { Id = Uri.from "http://nice.org.uk/ns/target1"
      Path = Path.from "qualitystandards/standard_1/statement_23.md"
      Content = "" }

  let nonMatchingTarget =
    { Id = Uri.from "http://nice.org.uk/ns/target1"
      Path = Path.from "qualitystandards/lol/standard_23.md"
      Content = "" }

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


:QualityStatement rdf:type <http://nice.org.uk/ns/compilation#FilePattern> ,
                           owl:NamedIndividual ;

                  <http://nice.org.uk/ns/compilation#expression> "statement_(?<QualityStatementId>.*).md"^^xsd:string ;

                  <http://nice.org.uk/ns/compilation#tool> <http://nice.org.uk/ns/compilation#Content> ;
                  <http://nice.org.uk/ns/compilation#represents> <http://nice.org.uk/ns/qualitystandard#QualityStatement>;

                  <http://nice.org.uk/ns/compilation#parent> <http://nice.org.uk/ns/compilation#QualityStandard> .


<http://nice.org.uk/ns/compilation#QualityStandard> rdf:type <http://nice.org.uk/ns/compilation#DirectoryPattern> ,
                                                              owl:NamedIndividual ;

                                                     <http://nice.org.uk/ns/compilation#expression> "standard_(?<QualityStandardId>.*)"^^xsd:string ;


                  <http://nice.org.uk/ns/compilation#parent> <http://nice.org.uk/ns/compilation#QualityStandards> .

    """
  let g = Graph.from qsCompilation
  let rp = loadMake g |> List.head
  test <@ toolsFor rp nonMatchingTarget = None @>
  test
    <@ toolsFor rp matchingTarget = Some({ Target = matchingTarget
                                           Tools = [ Content ]
                                           Captured =
                                             [ ("QualityStandardId", "1")
                                               ("QualityStatementId", "23") ] }) @>
