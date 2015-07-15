module Freya.Builder
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

