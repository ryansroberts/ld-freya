module Freya.Builder
open FSharp.RDF
open System.Collections.Generic

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

let private targets = Dictionary<_,_>()

let target (l:string) f =
  let t = (match f with
           | TargetFile x -> TargetFile {x with Id = targetUri l}
           | TargetDirectory x -> TargetDirectory {x with Id = targetUri l})
  targets.Add (l,t)
  t

let private dependencies = Dictionary<_,_>()

let rec hasDependents x xs =
  let t = targets.[x]
  let xs' = xs |> List.map (fun x -> (targets.[x]))
  match dependencies.ContainsKey t with
    | true -> dependencies.[t] <- xs' @ dependencies.[t]
    | false -> dependencies.Add (t,xs')
  x

let (===>) = hasDependents

open Yaaf.FSharp.Scripting
let exec xs =
  let xs' = List.map System.IO.File.ReadAllText xs
  let ax = System.AppDomain.CurrentDomain.GetAssemblies()
  let asm s = (Array.find (fun (x:System.Reflection.Assembly) -> x.FullName.StartsWith s ) ax).Location

  let fsi = ScriptHost.CreateNew()
  [
    for x in xs do
      let r = fsi.EvalScriptWithOutput x
      let targets = fsi.EvalExpression<Dictionary<string,TargetPattern>> "Freya.Builder.displayTargets"
      yield targets
  ]

let private resourcePathsS (dx:Dictionary<TargetPattern,_>) () =
  let kx = dx.Keys |> List.ofSeq
  let rec resourcePaths dpx kx = [
    for d' in dx.[List.head kx] do
      match d' with
      | TargetDirectory x -> yield! resourcePaths (x::dpx) (List.tail kx)
      | TargetFile x -> yield ResourcePath(dpx,x)
  ]
  resourcePaths [] kx

let resourcePaths = resourcePathsS dependencies
