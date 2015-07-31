module Freya.Builder

open FSharp.RDF
open System.Collections.Generic
open System

type TargetPattern =
  | TargetDirectory of DirectoryPattern
  | TargetFile of FilePattern

let private pandoc x = MarkdownConvertor(x) |> KnowledgeBaseProcessor
let docx = pandoc Docx
let pdf = pandoc Pdf
let html = pandoc HtmlFragment
let content = SemanticExtractor(Content)
let yamlMetadata = SemanticExtractor(YamlMetadata)

let targetUri =
  sprintf "http://ld.nice.org.uk/compilation/targets#%s" >> Uri.from

let file expression tools template (represents:string) =
  TargetFile { Id = Uri.from "re:placed"
               Expression = Expression.parse expression
               Tools = tools
               Template = template
               Represents = Uri.from represents}

let dir expression =
  TargetDirectory { Id = Uri.from "re:placed"
                    Expression = Expression.parse expression }

let private targets = Dictionary<_, _>()

let target (l : string) f =
  let t =
    (match f with
     | TargetFile x -> TargetFile { x with Id = targetUri l }
     | TargetDirectory x -> TargetDirectory { x with Id = targetUri l })
  targets.Add(l, t)
  t

let private dependencies = Dictionary<_, _>()
let private notRoot = List<_> ()

let rec hasDependents x xs =
  let t = targets.[x]
  let xs' = xs |> List.map (fun x -> (targets.[x]))
  notRoot.AddRange xs'
  match dependencies.ContainsKey t with
  | true -> dependencies.[t] <- xs' @ dependencies.[t]
  | false -> dependencies.Add(t, xs')
  x

let (===>) = hasDependents

open Yaaf.FSharp.Scripting

let displayTargets = targets
let displayDependencies = dependencies

type BuildScriptException(message:string, innerException:Exception) =
   inherit Exception(message, innerException)

let exec xs =
  let xs' = List.map System.IO.File.ReadAllText xs
  let ax = System.AppDomain.CurrentDomain.GetAssemblies()
  let asm s =
    (Array.find
       (fun (x : System.Reflection.Assembly) -> x.FullName.StartsWith s) ax).Location
  let fsi = ScriptHost.CreateNew()
  for x in xs do
    try
      fsi.EvalScriptWithOutput x |> ignore
    with
      | :? FsiEvaluationException as ev ->
        raise (BuildScriptException(ev.Result.Error.FsiOutput,ev))

  fsi.EvalExpression<ResourcePath list>
    "Freya.Builder.resourcePaths ()"


let private resourcePathsS (dx : Dictionary<_, _>) () =
  let roots = dx.Keys
              |> Seq.filter (notRoot.Contains >> not)
              |> List.ofSeq

  let rec resourcePaths dpx (TargetDirectory k) = [
      for d in dx.[TargetDirectory k] do
      match d with
      | TargetFile x -> yield ResourcePath(k::dpx |> List.rev, x)
      | TargetDirectory x -> yield! resourcePaths (k::dpx) d
  ]

  roots |> List.collect (resourcePaths [])

let resourcePaths = resourcePathsS dependencies
