#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharp.RDF/lib/net40/FSharp.RDF.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#r "../../packages/FSharp.Formatting/lib/net40/FSharp.Markdown.dll"
#r "../../packages/SharpYaml/lib/SharpYaml.dll"
#r "../../packages/ExtCore/lib/net40/ExtCore.dll"

#load "Model.fs"
#load "Yaml.fs"

open System.Text.RegularExpressions
open FSharp.RDF
open Swensen.Unquote
open FSharp.Markdown


let document = """
```
      prefix1:
         :property1
            - "Value 1"
            - "Value 2"
```
# F# Hello world
Hello world in [F#](http://fsharp.net) looks like this:

    printfn "Hello world!"

For more see [fsharp.org][fsorg].

  [fsorg]: http://fsharp.org "The F# organization." """
let parsed = Markdown.Parse(document)

parsed.Paragraphs

let y = """
qs:
  context:
    - "qualitystandard:Concept_Id"
    - "qualitystandard:AnotherConcept_Id"
dc:
  title:
    - "Some title text"
"""

Freya.YamlParser.parse y


open Freya
open Tracing
open Assertion
type YNode = Freya.YamlParser.Node

open YamlParser

let yamlMetadataAnnotation m =
    let translate = function
      | YNode.Map xs ->
        [ for (prefix, YNode.Map xs') in xs do
            for (property, List xs'') in xs' do
              for (Scalar n) in xs'' do
                let predicate = P(!(sprintf "%s:%s" prefix property))
                match n with
                | Scalar.String s ->
                  yield (predicate, O(Node.Literal(Literal.String s), lazy []))
                | Scalar.Uri u -> yield (predicate, O(Node.from u, lazy [])) ]

    let yamlToStatements y =
      try
        printfn "%s" y
        Success
          ([ info "Extracting metadata from yaml codeblock"
               (fileLocation m.Target.Path) ],
           [ rdf.resource m.Target.Id ( translate ( parse y ) )  ])
      with e ->
        Success
          ([ warn (sprintf "Failed to parse yaml: \r %s \r %s" y e.Message )
               (fileLocation m.Target.Path) ], [])

    let md = Markdown.Parse m.Target.Content
    match md.Paragraphs with
    | CodeBlock(yaml, _, _) :: _ -> yamlToStatements yaml
    | _ ->
      Success
        ([ warn "No metadata block at start of file"
             (fileLocation m.Target.Path) ], [])

let matchingTarget = {
      Id = Uri.from "http://nice.org.uk/ns/target1"
      ProvId = Uri.from "http://nice.org.uk/qualitystandards/resource"
      Path = Path.from "qualitystandards/standard_1/statement_23.md"
      Content = """
```
prefix1:
  property1:
     - "Value 1"
     - "Value 2"
```
#Some title

Hey this is markdown 
      """ }

let tm = { Target = matchingTarget
           Represents = (Uri.from "http://nice.org.uk/ns/qualitystandard#QualityStatement")
           Tools = [ Content ]
           Captured =
             [ ("QualityStandardId", "1")
               ("QualityStatementId", "23") ] }


let s = match yamlMetadataAnnotation tm with
  | Success(xw,xr) ->
    let sb = System.Text.StringBuilder ()
    let g = (graph.empty (!"http://nice.org.uk/ns/qualitystandard") [] )
    xr
    |> Assert.resources g
    |> graph.format (graph.write.ttl) (graph.toString sb)
    |> ignore

    sb.ToString()
