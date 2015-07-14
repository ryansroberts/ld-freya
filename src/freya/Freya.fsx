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

#load "Model.fs"
#load "Commands.fs"
open Freya
open FSharp.RDF
type TargetPattern =
  | TargetDirectory of  DirectoryPattern
  | TargetFile of FilePattern


let private pandoc x = MarkdownConvertor(x) |> KnowledgeBaseProcessor
let docx = pandoc Docx
let pdf = pandoc Pdf
let html = pandoc HtmlFragment


let targetUri = sprintf "http://ld.nice.org.uk/compilation/targets#%s" >> Uri.from

let file p xt t (r:string) = TargetFile {
  Id = Uri.from "re:placed"
  Expression = Expression.parse p
  Tools = xt
  Template = t
  Represents = Uri.from r
}

let dir p = TargetDirectory {
  Id = Uri.from "re:placed"
  Expression = Expression.parse p
}

let private targets = System.Collections.Generic.Dictionary<_,_>()

let target (l:string) f =
  let t = (match f with
           | TargetFile x -> TargetFile {x with Id = targetUri l}
           | TargetDirectory x -> TargetDirectory {x with Id = targetUri l})
  targets.Add (l,t)
  t

let private dependencies = System.Collections.Generic.Dictionary<_,_>()

let rec hasDependents x xs =
  let t = targets.[x]
  let xs' = xs |> List.map (fun x -> (targets.[x]))
  match dependencies.ContainsKey t with
    | true -> dependencies.[t] <- xs' @ dependencies.[t]
    | false -> dependencies.Add (t,xs')
  x

let (===>) = hasDependents

target "QualityStandardRoot" (dir "QualityStandards")
target "QualityStandards"    (dir "qs$(QualityStandardId)")
target "QualityStandard"     (file "QualityStandard.md"
                                   [docx;html]
                                   None
                                   "qs:QualityStandard")
target "QualityStatements"   (dir "st$(QualityStatementId)")
target "QualityStatement"    (file "Statement.md"
                                   [docx;html]
                                   None
                                   "qs:QualityStatement")

"QualityStandardRoot"
===> ["QualityStandards"
      ===> ["QualityStandard"
            "QualityStatements"
            ===> ["QualityStatement"]]]

dependencies




