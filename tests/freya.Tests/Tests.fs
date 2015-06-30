module freya.Tests
open Freya
open System.Text.RegularExpressions
open FSharp.RDF
open Xunit
open Swensen.Unquote
open compilation
open Tools
open TestSupport

let matchingTarget =
    { Id = Uri.from "http://ld.nice.org.uk/ns/target1"
      Specialisation = Uri.from "http://ld.nice.org.uk/qualitystandards/resource"
      Commit = Uri.from "http://ld.nice.org.uk/ns/prov/commit#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"
      Compilation = Uri.from "http://ld.nice.org.uk/ns/prov#compilation_2015-02-23T12:12:47.2583040+00:00"
      Path = File.from "qualitystandards/standard_1/statement_23.md"
      Content = (Uri.from "http://raw","") }

let nonMatchingTarget =
    { Id = Uri.from "http://ld.nice.org.uk/ns/target1"
      Specialisation = Uri.from "http://ld.nice.org.uk/qualitystandards/resource"
      Compilation = Uri.from "http://ld.nice.org.uk/ns/prov#compilation_2015-02-23T12:12:47.2583040+00:00"
      Commit = Uri.from "http://ld.nice.org.uk/ns/prov/commit#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"
      Path = File.from "qualitystandards/lol/standard_23.md"
      Content = (Uri.from "http://raw","") }

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
  :expression "statement_$(QualityStatementId).md"^^xsd:string ;
  :tool :Content ;
  :tool :YamlMetadata ;
  :represents :QualityStatement;
  :template "A template";
  :parent :QualityStandard .

:QualityStandard
  rdf:type :DirectoryPattern ,
           owl:NamedIndividual ;
  :expression "standard_$(QualityStandardId)"^^xsd:string ;
  :parent :QualityStandards .

"""

let g = Graph.loadTtl (fromString qsCompilation)
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

let provM = Graph.loadTtl (graph.fromString prov) |> loadProvenance

[<Fact>]
let ``Translate provenence to compilation targets`` () =

  provM.Id =? Uri.from "http://ld.nice.org.uk/ns/prov#compilation_2015-02-23T12:12:47.2583040+00:00"
  let commits = provM.Commits |> List.ofSeq
  let targets = provM.Targets |> List.ofSeq
  commits       =?     [{Id = (Uri.from "http://ld.nice.org.uk/ns/prov/commit#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd")
                         When = "2015-02-23T12:12:47.259270+00:00"}
                        {Id = (Uri.from "http://ld.nice.org.uk/ns/prov/commit#999586c1dfe8a71c6cbf6c129f404c5642ff31bd")
                         When = "2015-02-23T12:12:47.259270+00:00"}]
  targets       =? [{Id = Uri.from "http://ld.nice.org.uk/ns/prov/entity#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"
                     Specialisation = Uri.from "http://ld.nice.org.uk/ns/prov/new.md"
                     Compilation = Uri.from "http://ld.nice.org.uk/ns/prov#compilation_2015-02-23T12:12:47.2583040+00:00"
                     Commit = Uri.from "http://ld.nice.org.uk/ns/prov/commit/c47800c"
                     Path = File.from "qualitystandards/standard_1/statement_23.md"
                     Content = (Uri.from "file:///testrepo/content.md","")}]

let res = makeAll [rp] provM.Targets |> Array.ofSeq
[<Fact>]
let ``Execute specified tools on compilation targets to produce ontology`` () =
  let x = match res with [|PipelineExecution.Success(t,{Provenance=_;Extracted=x;})|] -> x
  x <>? []


let yamlContent =
    """
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
      """

let matchingYamlTarget = {
      Id = Uri.from "http://ld.nice.org.uk/ns/target1"
      Specialisation = Uri.from "http://ld.nice.org.uk/qualitystandards/resource:version"
      Commit = Uri.from "http://ld.nice.org.uk/ns/prov/commit#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"
      Compilation = Uri.from "http://ld.nice.org.uk/ns/prov#compilation_2015-02-23T12:12:47.2583040+00:00"
      Path = File.from "qualitystandards/standard_1/statement_23.md"
      Content = (Uri.from "http://raw",yamlContent)}

let tm = { Target = matchingYamlTarget
           Represents = (Uri.from "http://ld.nice.org.uk/ns/qualitystandard#QualityStatement")
           Tools = [ SemanticExtractor(Content) ]
           Captured =[] }

open resource
[<Fact>]
let ``Extract arbitrary statements from YAML metadata`` () =
  let r = Tools.yamlMetadata (SemanticExtractor(YamlMetadata)) (PipelineStep (tm,[]))  |> Async.RunSynchronously
  match r with
    | PipelineStep(t,[ToolExecution.Success{Provenance=[d;g];Extracted=r::rx}]) ->
      match r with
        | DataProperty (Uri.from "prefix:property") xsd.string [v1;v2] ->
          [v1;v2] =? ["Value 1";"Value 2"]
      match r with
        | ObjectProperty (Uri.from "prefix:objectProperty") [x] ->
          x =? Uri.from "prefix:fragment"
      match d with
        | ObjectProperty (Uri.from "prov:wasDerivedFrom") [x] ->
          x =? matchingYamlTarget.Id

open Commands
open Freya
open Assertion
open rdf

[<Fact>]
let ``Getting a description of a filepath`` () =
  let qs = describe [rp] (Path.from "qualitystandards/*") |> Seq.toList
  let g = Graph.empty !"http://ld.nice.org.uk" []
  let g' = Graph.empty !"http://ld.nice.org.uk" []
  [rdf.resource !"http://ld.nice.org.uk/command" [
         a !"http://ld.nice.org.uk/ns/compilation/Command"
         objectProperty !"compilation:represents" !"http://ld.nice.org.uk/ns/compilation#QualityStatement"
         objectProperty !"compilation:tool" !"compilation:Content"
         objectProperty !"compilation:tool" !"compilation:YamlMetadata"
         ]]
  |> Assert.graph g
  |> ignore

  Assert.graph g qs |> ignore

  graphsAreSame g g
