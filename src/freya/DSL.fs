module Freya.Builder

open Freya
open FSharp.RDF
open System.Collections.Generic
open System
open FSharp.RDF.Assertion

type Tool = ToolMatch -> ToolExecution

type TargetPattern =
  | TargetDirectory of DirectoryPattern
  | TargetFile of FilePattern

type ExtractionContext<'a> =
  { Represents : FSharp.RDF.Uri
    TargetId : FSharp.RDF.Uri
    Path : File
    Content : 'a
    Captured : Capture list}

type ExtractionResult =
  { Trace : Statement list
    Extracted : Resource list }

//Naughty global mutable state, could refactor but it simplifies the implementation quite a bit
let private tools = Dictionary<_, _>()
let private dependencies = Dictionary<_, _>()
let private notRoot = List<_>()
let private targets = Dictionary<_, _>()

tools.Add("Docx", Pandoc Docx)
tools.Add("Pdf", Pandoc Pdf)
tools.Add("HtmlFragment", Pandoc HtmlFragment)
tools.Add("HtmlDocument", Pandoc HtmlFragment)

let extractor n f =
  let t =
    SemanticExtractor
      (Name n,
       ToolFn
         (fun m xr ->
         match f ({ Represents = m.Represents
                    TargetId = m.Target.Id
                    Path = m.Target.Path
                    Content = m.Target.Content |> snd
                    Captured = m.Captured}) with
         | { Trace = trace; Extracted = extracted } ->
           pipeline.succeed (Tracing.semanticExtraction m (Name n) trace)
             extracted))
  tools.Add(n, t)
  t

//Built in extractors
let content =
  extractor "Content"
    (fun x ->
    { Trace =
        [ Tracing.info "Extrated content from raw" (Tracing.fileLocation x.Path) ]
      Extracted =
        [ owl.individual x.TargetId [ x.Represents ]
            [ rdf.dataProperty !!"http://www.w3.org/2011/content#chars"
                (x.Content ^^ xsd.string) ] ] })

open Freya.YamlParser
open FSharp.Markdown

let markdownExtractor n f =
  extractor n (fun x ->
    f { Represents = x.Represents
        TargetId = x.TargetId
        Path = x.Path
        Content = Markdown.Parse x.Content
        Captured = x.Captured})

let yamlExtractor n f =
  extractor n (fun x ->
    let md = Markdown.Parse x.Content

    let yamlBlock =
      function
      | CodeBlock(yaml, _, _) -> Some yaml
      | _ -> None
    match md.Paragraphs |> List.choose yamlBlock with
    | yaml :: _ ->
      try
        f { Represents = x.Represents
            TargetId = x.TargetId
            Path = x.Path
            Content = YamlParser.parse yaml
            Captured = x.Captured}
      with :? SharpYaml.YamlException as e ->
        { Trace =
            [ Tracing.warn (sprintf "Yaml parse error %s" e.Message)
                (Tracing.fileLocation x.Path) ]
          Extracted = [] }
    | _ ->
      { Trace =
          [ Tracing.warn
              (sprintf "No valid yaml block at start of file %s \n---- %A"
                 x.Content md.Paragraphs) (Tracing.fileLocation x.Path) ]
        Extracted = [] })

let targetUri =
  sprintf "http://ld.nice.org.uk/compilation/targets#%s" >> Uri.from

let file expression tx template (represents : string) =
  TargetFile { Id = Uri.from "re:placed"
               Expression = Expression.parse expression
               Tools = List.map (fun x -> tools.[x]) tx
               Template = template
               Represents = Uri.from represents }

let dir expression =
  TargetDirectory { Id = Uri.from "re:placed"
                    Expression = Expression.parse expression }

let target (l : string) f =
  let t =
    (match f with
     | TargetFile x -> TargetFile { x with Id = targetUri l }
     | TargetDirectory x -> TargetDirectory { x with Id = targetUri l })
  targets.Add(l, t)
  t

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

type BuildScriptException(message : string, innerException : Exception) =
  inherit Exception(message, innerException)

let exec xs =
  printfn "Discovered build script at %A" xs
  let fsi = ScriptHost.CreateNew()
  for x in xs do
    try
      printfn "Load build script %s" x
      fsi.Load x
    with :? FsiEvaluationException as ev ->
      raise
        (BuildScriptException
           (sprintf "Failed to evaluate %s\r%s" x ev.Result.Error.FsiOutput, ev))
  fsi.EvalExpression<ResourcePath list> "Freya.Builder.resourcePaths ()"

let private resourcePathsS (dx : Dictionary<_, _>) () =
  let roots =
    dx.Keys
    |> Seq.filter (notRoot.Contains >> not)
    |> List.ofSeq

  let rec resourcePaths dpx (TargetDirectory k) =
    [ for d in dx.[TargetDirectory k] do
        match d with
        | TargetFile x -> yield ResourcePath(k :: dpx |> List.rev, x)
        | TargetDirectory x -> yield! resourcePaths (k :: dpx) d ]

  roots |> List.collect (resourcePaths [])

let resourcePaths = resourcePathsS dependencies
