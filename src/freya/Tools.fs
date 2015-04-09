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
  let content m  =
    Success ([info "Apply content tool" (fileLocation m.Target.Path) ],
             [ owl.individual m.Target.Id [ m.Represents ]
             [ dataProperty !"cnt:chars" (m.Target.Content ^^ xsd.string) ] ])

  type YNode = Freya.YamlParser.Node
  let yamlMetadataAnnotation m t =
    let translate = function
        | YNode.Map xs -> [
          for (prefix,YNode.Map xs') in xs do
          for (property,List xs'') in xs' do
          for (Scalar n) in xs'' do
          let subject = S(!(sprintf "%s:%s" prefix property))
          match n with
          | Scalar.String s -> yield (subject,O(Node.Literal ( Literal.String s),lazy []))
          | Scalar.Uri u -> yield (subject,O(Node.from u,lazy []))
        ]


    let yamlToStatements y =
      try
        Success([info "Extracting metadata from yaml codeblock" (fileLocation t.Path)],[rdf.resource t.ProvId [ parse y |> translate ]])
      with
      | e -> Success([warn (sprintf "Failed to parse yaml: %s" e.Message) (fileLocation t.Path)],[])
    let md = Markdown.Parse m.Target.Content
    match md.Paragraphs with
      | CodeBlock (yaml,_,_)::_ -> yamlToStatements yaml
      | _ -> Success([warn "No metadata block at start of file" (fileLocation m.Target.Path)],[])

  let exec t =
    match t with
    | Content -> content

  let execMatches xtm =
    match xtm with
    | [ x ] ->
      [ for tl in x.Tools -> x |> exec tl ]
    | x :: xs -> [ Failure ([],[])  ]
    | [] -> []

  let make xrp t =
    xrp
    |> List.map (toolsFor t)
    |> List.choose id
    |> execMatches

  let makeAll xrp xt = xt |> List.collect (make xrp)
