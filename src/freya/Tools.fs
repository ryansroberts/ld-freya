namespace Freya

open FSharp.RDF
open FSharp.Markdown
open Assertion
open rdf
open owl
open Tracing
open YamlParser
open ExtCore
open ExtCore.Collections

module Tools =
  let either =
    function
    | ToolExecution.Failure x -> x
    | ToolExecution.Success x -> x

  let step f (PipelineStep(m, xp)) =
    async {
      let xr =
        xp
        |> List.map either
        |> List.collect (fun { Provenance = _; Extracted = r } -> r)
      let! res = f m xr
      return PipelineStep(m, res :: xp)
    }

  let private contentS m _ =
    async
      {
      return pipeline.succeed
               [ info "Apply content tool" (fileLocation m.Target.Path) ]
               [ owl.individual m.Target.Id [ m.Represents ]
                   [ dataProperty !"cnt:chars" (m.Target.Content ^^ xsd.string) ] ] }
  let content = step contentS

  type YNode = Freya.YamlParser.Node

  let yamlMetadataS m _ =
    async {
      let translate = function
        | YNode.Map xs ->
          [ for (prefix, YNode.Map xs') in xs do
              for (property, List xs'') in xs' do
                for (Scalar n) in xs'' do
                  let predicate = P(!(sprintf "%s:%s" prefix property))
                  match n with
                  | Scalar.String s ->
                    yield (predicate, O(Node.Literal(Literal.String s), lazy []))
                  | Scalar.Uri u -> yield (predicate, O(Node.from u, lazy [])) ]

      let yamlToStatements y =
        try
          pipeline.succeed
            [ info "Extracting metadata from yaml codeblock"
                (fileLocation m.Target.Path) ]
            [ rdf.resource m.Target.Id (translate (parse y)) ]
        with e ->
          pipeline.succeed
            [ warn (sprintf "Failed to parse yaml: %s" e.Message)
                (fileLocation m.Target.Path) ] []

      let md = Markdown.Parse m.Target.Content
      match md.Paragraphs with
      | CodeBlock(yaml, _, _) :: _ -> return yamlToStatements yaml
      | _ ->
        return pipeline.succeed
                 [ warn "No metadata block at start of file"
                     (fileLocation m.Target.Path) ] []
    }

  let yamlMetadata = step yamlMetadataS

  open Pandoc

  let convertMarkdownS t m xr =
    match xr with
    | r :: xr -> async { let! prov = convertResources r [] t
                         return pipeline.succeed prov [ r ] }
    | [] -> async { return pipeline.fail [] }

  let fragment (Uri.Sys u) = u.Fragment.Substring(1, u.Fragment.Length - 1)

  let convertMarkdown x t =
    step (convertMarkdownS (x,
                            { Output = Path.from "/artifacts/"
                              Commit = fragment t.Target.Commit
                              WorkingDir = Path.from "." }))

  let exec t =
    function
    | SemanticExtractor(Content) -> content
    | SemanticExtractor(YamlMetadata) -> yamlMetadata
    | KnowledgeBaseProcessor(MarkdownConvertor x) -> convertMarkdown x t

  let composeStep a b = (fun x -> async { let! r = a x
                                          return! b r })

  //Produce a single PipelineStep -> PipelineStep from the tool list to apply to the source
  let execMatches x =
    async {
      return! (x.Tools
               |> List.map (exec x)
               |> List.reduce composeStep) (PipelineStep(x, []))
    }

  let make xrp t =
    xrp
    |> List.map (compilation.toolsFor t)
    |> List.choose id
    |> List.map execMatches

  open FSharp.Collections.ParallelSeq

  let makeAll xrp xt =
    xt
    |> PSeq.collect (make xrp)
    |> PSeq.map Async.RunSynchronously
    |> PSeq.map pipeline.result
