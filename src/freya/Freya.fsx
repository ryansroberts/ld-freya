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
#r "../../Packages/FSharp.Compiler.Service/lib/net40/FSharp.Compiler.Service.dll"
#r "System.Xml.Linq.dll"
#load "../../paket-files/matthid/Yaaf.FSharp.Scripting/src/source/Yaaf.FSharp.Scripting/YaafFSharpScripting.fs"
#load "Model.fs"

open Freya
#load "Commands.fs"
#load "DSL.fs"

open Freya.Builder
target "Root" (dir "root")
target "QualityStandards" (dir "qualitystandards")
target "QualityStatement" (file "statement_$(QualityStandardId).md"
                                [content;yamlMetadata;]
                                (Some "file://templates/qs.md")
                                "http://ld.nice.org.uk/ns/qualitystandards/QualityStatement")
target "QualityStandard"  (dir "standard_$(QualityStandardId)")
target "QualityStandardDoc" (file "Standard.md"
                                  [content;yamlMetadata]
                                  (Some "file://")
                                  "http://ld.nice.org.uk/ns/QualityStandards/QualityStandard")
"Root"
===> ["QualityStandards"
      ===> ["QualityStandardDoc"
            "QualityStandard"
            ===> ["QualityStatement"]]]

let rp = Freya.Builder.resourcePaths ()


