#I "../../bin"
#r "../../packages/json-ld.net/lib/net40-Client/JsonLD.dll"
#r "../../packages/FSharp.RDF/lib/FSharp.RDF.dll"
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
#r "../../packages/ExtCore/lib/net40/ExtCore.dll"
#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/UnionArgParser/lib/net40/UnionArgParser.dll"
#r "../../packages/SharpYaml/lib/SharpYaml.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r "../../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
#r "../../bin/freya.publish.exe"

open FSharp.Data
open Newtonsoft.Json.Linq
open FSharp.RDF

let commit = Uri.from (sprintf "http://ld.nice.org.uk/prov/commit#%s" "824b345")
let stardog = 
  Store.Store.stardog "http://192.168.99.100/" "nice" "admin" "admin"
let contexts = 
  [ "http://192.168.99.100/ns/qualitystandard.jsonld"; 
    "http://192.168.99.100/ns/qualitystandard/agegroup.jsonld"; 
    "http://192.168.99.100/ns/qualitystandard/conditiondisease.jsonld"; 
    "http://192.168.99.100/ns/qualitystandard/setting.jsonld"; 
    "http://192.168.99.100/ns/qualitystandard/servicearea.jsonld"; 
    "http://192.168.99.100/ns/prov.jsonld"; 
    "http://192.168.99.100/ns/owl.jsonld"; 
    "http://192.168.99.100/ns/dcterms.jsonld" ]

open Freya.Publication

publish stardog commit 
  [ "<http://ld.nice.org.uk/ns/qualitystandard#setting>"; 
    "<http://ld.nice.org.uk/ns/qualitystandard#targetPopulation>" ] contexts
|> Seq.toList
|> List.map string
