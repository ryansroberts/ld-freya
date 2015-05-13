module freya.Tests
open Freya
open System.Text.RegularExpressions
open FSharp.RDF
open Xunit
open Swensen.Unquote
open compilation
open Tools

let matchingTarget =
    { Id = Uri.from "http://ld.nice.org.uk/ns/target1"
      ProvId = Uri.from "http://ld.nice.org.uk/qualitystandards/resource"
      Path = toPath "qualitystandards/standard_1/statement_23.md"
      Content = "" }

let nonMatchingTarget =
    { Id = Uri.from "http://ld.nice.org.uk/ns/target1"
      ProvId = Uri.from "http://ld.nice.org.uk/qualitystandards/resource"
      Path = toPath "qualitystandards/lol/standard_23.md"
      Content = "" }

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

let g = graph.loadFormat graph.parse.ttl (graph.fromString qsCompilation)
let rp = loadMake g |> List.head

loader <- (fun s -> "")

[<Fact>]
let ``Tools fail to match unless correctly configured``() =
  test <@ toolsFor nonMatchingTarget rp = None @>
let ``Match provenence entities to compilation tools``() =
  printf "Tools - %A" ( toolsFor matchingTarget rp  ) |> id
  test <@ toolsFor matchingTarget rp = Some({ Target = matchingTarget
                                              Represents = (Uri.from "http://ld.nice.org.uk/ns/qualitystandard#QualityStatement")
                                              Tools = [SemanticExtractor(YamlMetadata);SemanticExtractor(Content)]
                                              Captured =
                                                [ ("QualityStandardId", "1")
                                                  ("QualityStatementId", "23") ] }) @>

let prov = """@base <http://ld.nice.org.uk/ns/compilation>.

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.
@prefix prov: <http://www.w3.org/ns/prov#>.
@prefix owl: <http://www.w3.org/2002/07/owl#>.
@prefix compilation: <http://ld.nice.org.uk/ns/compilation#>.
@prefix cnt: <http://www.w3.org/2011/content#>.
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

<http://ld.nice.org.uk/ns/prov/commit#999586c1dfe8a71c6cbf6c129f404c5642ff31bd>
  a prov:Commit;
  compilation:path "qualitystandards/standard_1/statement_24.md";
  compilation:content <file:///testrepo/content.md>;
  prov:specializationOf <http://ld.nice.org.uk/ns/prov/new2.md>;
  prov:startedAtTime "2015-02-23T12:12:47.259270+00:00"^^xsd:dateTime;
  prov:wasAttributedTo <http://ld.nice.org.uk/ns/prov/user/schacon@gmail.com>;
  prov:wasGeneratedBy <http://ld.nice.org.uk/ns/prov/commit/c47800c>.

<http://ld.nice.org.uk/ns/prov/commit#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd>
  a prov:Commit;
  prov:informedBy <http://ld.nice.org.uk/ns/prov/commit#999586c1dfe8a71c6cbf6c129f404c5642ff31bd> ;
  compilation:path "qualitystandards/standard_1/statement_23.md";
  compilation:content <file:///testrepo/content.md>;
  prov:startedAtTime "2015-02-23T12:12:47.259270+00:00"^^xsd:dateTime;
  prov:specializationOf <http://ld.nice.org.uk/ns/prov/new.md>;
  prov:wasAttributedTo <http://ld.nice.org.uk/ns/prov/user/schacon@gmail.com>;
  prov:wasGeneratedBy <http://ld.nice.org.uk/ns/prov/commit/c47800c>.

<http://ld.nice.org.uk/ns/prov#compilation_2015-02-23T12:12:47.2583040+00:00>
  a <http://ld.nice.org.uk/ns/compilation#Compilation> ;
  rdfs:label "Change this to a compilation message that is actualy useful to somebody";
  prov:informedBy <http://ld.nice.org.uk/ns/prov/commit#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd>;
  prov:qualifiedAssociation [a prov:Association ;
                             prov:agent <http://ld.nice.org.uk/ns/prov#user/ryanroberts> ;
                             prov:hadRole "initiator"];
  prov:startedAtTime "2015-02-23T12:12:47.259270+00:00"^^xsd:dateTime;
  prov:uses <http://ld.nice.org.uk/ns/prov/entity#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd>;
  prov:wasAssociatedWith <http://ld.nice.org.uk/ns/prov/user/ryanroberts>.

<http://ld.nice.org.uk/ns/prov/entity#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd>
  a prov:Entity;
  compilation:path "qualitystandards/standard_1/statement_23.md";
  compilation:content <file:///testrepo/content.md>;
  prov:specializationOf <http://ld.nice.org.uk/ns/prov/new.md>;
  prov:wasAttributedTo <http://ld.nice.org.uk/ns/prov/user/schacon@gmail.com>;
  prov:wasGeneratedBy <http://ld.nice.org.uk/ns/prov/commit/c47800c>.
"""

let provM = graph.loadFormat graph.parse.ttl (graph.fromString prov) |> loadProvenance

[<Fact>]
let ``Translate provenence to compilation targets`` () =

  provM.Id =? Uri.from "http://ld.nice.org.uk/ns/prov#compilation_2015-02-23T12:12:47.2583040+00:00"
  provM.Commits =?     [{Id = (Uri.from "http://ld.nice.org.uk/ns/prov/commit#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd")
                         When = "2015-02-23T12:12:47.259270+00:00"}
                        {Id = (Uri.from "http://ld.nice.org.uk/ns/prov/commit#999586c1dfe8a71c6cbf6c129f404c5642ff31bd")
                         When = "2015-02-23T12:12:47.259270+00:00"}]
  provM.Targets =? [{Id = Uri.from "http://ld.nice.org.uk/ns/prov/new.md"
                     ProvId = Uri.from "http://ld.nice.org.uk/ns/prov/entity#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"
                     Path = Path [Segment "qualitystandards"; Segment "standard_1"; Segment "statement_23.md"]
                     Content = ""}]

let res = makeAll [rp] provM.Targets
[<Fact>]
let ``Execute specified tools on compilation targets to produce ontology`` () =
  let x = match res with [|PipelineExecution.Success(t,{Provenence=_;Extracted=x;})|] -> x
  x <>? []

let matchingYamlTarget = {
      Id = Uri.from "http://ld.nice.org.uk/ns/target1"
      ProvId = Uri.from "http://ld.nice.org.uk/qualitystandards/resource"
      Path = toPath "qualitystandards/standard_1/statement_23.md"
      Content = """
```
prefix:
  property:
     - "Value 1"
     - "Value 2"
  objectProperty:
     - "prefix:fragment"
```
#Some title

Hey this is markdown
      """ }

let tm = { Target = matchingYamlTarget
           Represents = (Uri.from "http://ld.nice.org.uk/ns/qualitystandard#QualityStatement")
           Tools = [ SemanticExtractor(Content) ]
           Captured =[] }

open resource

[<Fact>]
let ``Extract arbitrary statements from YAML metadata`` () =
  let r = Tools.yamlMetadata (PipelineStep (tm,[]))  |> Async.RunSynchronously
  match r with
    | PipelineStep(t,[ToolExecution.Success{Provenence=xe;Extracted=r::rx}]) ->
      match r with
        | DataProperty (Uri.from "prefix:property") xsd.string [v1;v2] ->
          [v1;v2] =? ["Value 1";"Value 2"]
      match r with
        | ObjectProperty (Uri.from "prefix:objectProperty") [x] ->
          x =? Uri.from "prefix:fragment"
