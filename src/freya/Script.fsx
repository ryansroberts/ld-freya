#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharp.RDF/lib/net40/FSharp.RDF.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#r "../../packages/FSharp.Formatting/lib/net40/FSharp.Markdown.dll"
#r "../../packages/SharpYaml/lib/SharpYaml.dll"
#r "../../packages/ExtCore/lib/net40/ExtCore.dll"
#r "../../packages/UnionArgParser/lib/net40/UnionArgParser.dll"

#load "Model.fs"
open Freya.path
#load "Yaml.fs"
#load "GuardedAwaitObservable.fs"
#load "Pandoc.fs"

open Freya
open FSharp.RDF
open Assertion
open rdf
let r = owl.individual !"http://nice.org.uk/things#1" [] [
         dataProperty !("content:chars") ( """
# Hi I am markdown

Text etc
"""^^xsd.string)
     ]


[Pandoc.Pdf;Pandoc.HtmlDocument;Pandoc.Docx]
|> List.map (fun o->
async {
  let! (_,O (m,p)) =  Pandoc.convertResources r [] (o,{Output = toPath __SOURCE_DIRECTORY__;WorkingDir = toPath __SOURCE_DIRECTORY__})
  printfn "%A %A" m p.Value
}) |> Async.Parallel |> Async.RunSynchronously

