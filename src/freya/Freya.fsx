#r "../../packages/json-ld.net/lib/net40-Client/JsonLD.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/ExtCore/lib/net40/ExtCore.dll"
#I "../../packages/FParsec/lib/net40-client"
#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#I "../../packages/FSharp.RDF/lib/"
#r "../../packages/FSharp.RDF/lib/owlapi.dll"
#r "../../packages/FSharp.RDF/lib/FSharp.RDF.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../../packages/FSharp.Formatting/lib/net40/FSharp.Markdown.dll"
#I "../../packages/FSharp.RDF/lib"
#I "../../packages"
#r "../../packages/ExtCore/lib/net40/ExtCore.dll"
#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/UnionArgParser/lib/net40/UnionArgParser.dll"
#r "../../packages/SharpYaml/lib/SharpYaml.dll"
#r "../../packages/FSharp.RDF/lib/FSharp.RDF.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
#r "../../packages/FSharp.Compiler.Service/lib/net40/FSharp.Compiler.Service.dll"
#load "Model.fs"
#load "Yaml.fs"
#load "GuardedAwaitObservable.s"
#load "Pandoc.fs"
#load "../../paket-files/matthid/Yaaf.FSharp.Scripting/src/source/Yaaf.FSharp.Scripting/YaafFSharpScripting.fs"
#load "Tools.fs"
#load "DSL.fs"

open Freya
open Freya.Builder
open Freya.YamlParser
open FSharp.RDF

let mkKey (x : string) = x.Replace(" ", "").ToLowerInvariant()

let vocabLookup uri =
  let rdfslbl = Uri.from "http://www.w3.org/2000/01/rdf-schema#label"
  let gcd = Graph.loadFrom uri
  let onlySome = List.filter Option.isSome >> List.map Option.get
  Resource.fromPredicate rdfslbl gcd
  |> List.map (fun r ->
       match r with
       | FunctionalDataProperty rdfslbl xsd.string x ->
         Some(mkKey x, Resource.id r)
       | r -> None)
  |> onlySome
  |> Map.ofList

let lookupVocab =
  ([ "setting",
     vocabLookup "http://192.168.59.103/ns/qualitystandard/setting.ttl"

     "agegroup",
     vocabLookup "http://192.168.59.103/ns/qualitystandard/agegroup.ttl"

     "lifestylecondition",
     vocabLookup
       "http://192.168.59.103/ns/qualitystandard/lifestylecondition.ttl"

     "conditiondisease",
     vocabLookup "http://192.168.59.103/ns/qualitystandard/conditiondisease.ttl"

     "servicearea",
     vocabLookup "http://192.168.59.103/ns/qualitystandard/servicearea.ttl" ]
   |> Map.ofList)

let lookupProperty =
  ([ "setting", Uri.from "http://ld.nice.org.uk/ns/qualitystandard#setting"

     "agegroup",
     Uri.from "http://ld.nice.org.uk/ns/qualitystandard#targetPopulation"

     "conditiondisease",
     Uri.from "http://ld.nice.org.uk/ns/qualitystandard#targetPopulation"

     "servicearea",
     Uri.from "http://ld.nice.org.uk/ns/qualitystandard#serviceArea"

     "lifestylecondition",
     Uri.from "http://ld.nice.org.uk/ns/qualitystandard#targetPopulation" ]
   |> Map.ofList)

type YNode = Freya.YamlParser.Node

open FSharp.RDF.Assertion
open rdf

let owlAllValuesFrom property  = function
  | [] -> []
  | ranges -> [for range in ranges -> 
  blank !!"rdfs:subClassOf"
    [ a !!"owl:Restriction"
      objectProperty !!"owl:onProperty" property
      objectProperty !!"owl:allValuesFrom" range]
  ]

let qsAnnotations ctx =
  let message f x = f x (Tracing.fileLocation ctx.Path)
  let info = message Tracing.info
  let warn = message Tracing.warn
  let onlySome = List.filter Option.isSome >> List.map Option.get

  //Pairs of trace message / annotation uri
  let lookUpScalar vocabKey =
    function
    | Node.Scalar(Scalar.String term) ->
      printfn "%A %A" vocabKey term
      match Map.tryFind (mkKey vocabKey) lookupVocab with
      | Some vocab ->
        match Map.tryFind (mkKey term) vocab with
        | Some uri -> (info (sprintf "Annotating for term %s" term), Some uri)
        | None -> (warn (sprintf "Cannot find '%s' in '%s'" term vocabKey), None)
      | None -> (warn (sprintf "Cannot find vocabulary '%s'" vocabKey), None)
    | _ -> (warn (sprintf "Malformed yaml"), None)

  let extracted =
    match ctx.Content with
    | Map xs ->
      xs
      |> List.map (function
           | k, YNode.List xv ->
             (Map.tryFind (mkKey k) lookupProperty,
              List.map (lookUpScalar (mkKey k)) xv)
           | k, _ -> (None, []))
      |> List.map (function
           | Some k, xs ->
             (List.map fst xs,
              owlAllValuesFrom k ((List.map snd xs |> onlySome)) )
           | _, xs -> (List.map fst xs, []))


  { Trace = List.concat (List.map fst extracted)
    Extracted =  List.map snd extracted
                 |> List.map (owl.cls ctx.TargetId [] )}

let r = (qsAnnotations {
  Represents =
                  !!"http://ld.nice.org.uk/ns/qualitystandard#QualityStandard"
  TargetId =
                  !!"http://ld.nice.org.uk/entity#3decd:/qualitystandards/qs1/st2/Statement.md"
  Path = File.from "qualitystandards/qs1/st2/Statement.md"
  Content = YamlParser.parse """
Setting:
  - "Outpatient clinic"
Age Group:
  - "Adults 18-24"
  - "Adults 25-64"
  - "Young people"
Service area:
  - "Maternity care"
Condition disease:
  - "Pregnancy"
Lifestyle condition:
  - "Physical activity"
  - "Unhealthy eating habits"
  """ })

let g = Graph.unnamed []

r.Extracted
|> Assert.graph g
|> Graph.print
