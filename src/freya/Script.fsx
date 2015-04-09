#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharp.RDF/lib/net40/FSharp.RDF.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#r "../../packages/FSharp.Formatting/lib/net40/FSharp.Markdown.dll"
#r "../../packages/SharpYaml/lib/SharpYaml.dll"

#load "Yaml.fs"
open System.Text.RegularExpressions
open FSharp.RDF
open Swensen.Unquote
open FSharp.Markdown


let document = """
```
some:
   yaml:
     - here
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
