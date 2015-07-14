

module Freya.Builder
open FSharp.RDF
type TargetDirectory =
  | TargetDirectory of DirectoryPattern
  | TargetFile of FilePattern


let private pandoc x = MarkdownConvertor(x) |> KnowledgeBaseProcessor
let docx = pandoc Docx
let pdf = pandoc Pdf
let html = pandoc HtmlFragment


let file p xt t r = {
  Id = Uri.from "lol"
  Expression = Expression.parse p
  Tools = xt
  Template = t
  Represents = r
}

let dir p xs = ()






