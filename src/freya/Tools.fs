namespace Freya

open FSharp.RDF
open FSharp.Markdown
open Assertion
open rdf
open owl
open compilation
open Tracing
open YamlParser

module tools =

  
  let content (m:ToolMatch) : ToolExecution =
    Success {
      Provenence = [info "Apply content tool" (fileLocation m.Target.Path)]
      Extracted = [owl.individual m.Target.Id [m.Represents] [ dataProperty !"cnt:chars" (m.Target.Content ^^ xsd.string) ]]
      Target = m.Target }

  type YNode = Freya.YamlParser.Node

  let yamlMetadata (m:ToolMatch) : ToolExecution =
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
        Success {
          Provenence = [info "Extracting metadata from yaml codeblock" (fileLocation m.Target.Path) ]
          Extracted = [rdf.resource m.Target.Id ( translate ( parse y ) )]
          Target = m.Target
          }
      with e ->
        Success {
          Provenence = [warn (sprintf "Failed to parse yaml: %s" e.Message) (fileLocation m.Target.Path)]
          Extracted = []
          Target = m.Target
          }

    let md = Markdown.Parse m.Target.Content
    match md.Paragraphs with
    | CodeBlock(yaml, _, _) :: _ -> yamlToStatements yaml
    | _ ->
      Success {
          Provenence = [ warn "No metadata block at start of file" (fileLocation m.Target.Path) ]
          Extracted = []
          Target = m.Target
        }

  let exec t =
    match t with
    | Content -> content
    | YamlMetadata -> yamlMetadata

  let execMatches xtm =
    match xtm with
    | [ x ] ->
      [ for tl in x.Tools -> x |> exec tl ]
    | x :: xs -> [ Failure({Provenence = [];Extracted = [];Target = x.Target}) ]
    | [] -> []

  let make xrp t =
    xrp
    |> List.map (toolsFor t)
    |> List.choose id
    |> execMatches

  let makeAll xrp xt = xt |> List.collect (make xrp)
