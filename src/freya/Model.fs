namespace Freya

open VDS.RDF
open System.Text.RegularExpressions
open FSharp.RDF
open resource
open ExtCore

type Segment =
  | Segment of string
  override x.ToString() =
    let (Segment s) = x
    s

type Path =
  | Path of Segment list
  static member (++) ((Path a), (Path b)) = Path(a @ b)
  static member (++) (p, f) = File(p, f)
  override x.ToString() =
    match x with
    | Path xs -> System.String.Join("/", xs)
  static member from s =
    String.split [|'/'|] s
    |> Array.map Segment
    |> List.ofArray
    |> Path

and FileName = string

and Extension = string

and FullName =
  | FullName of (FileName * Extension)

and File =
  | File of Path * FullName
  override x.ToString() =
    match x with
    | File(p, FullName(f, e)) -> sprintf "%s/%s.%s" (string p) f e

[<AutoOpen>]
module path =
  let ensurePathExists (File(p, f)) =
    let d = System.IO.DirectoryInfo(string p)
    d.Create()
    File(p, f)

  let toPath s =
    String.split [| '/' |] s
    |> Array.map Segment
    |> Array.toList
    |> Path.Path

type Target =
  { Id : Uri
    ProvId : Uri
    Path : Path
    Content : string }

type Commit =
  { Id : Uri
    When : string }

type Provenance =
  { Id : Uri
    Commits : Commit list
    Targets : Target list }

type SemanticExtractor =
  | Content
  | YamlMetadata
  override x.ToString () =
    match x with
      | Content -> "Content"
      | YamlMetadata -> "YamlMetadata"

type MarkdownConvertor =
  | HtmlDocument
  | HtmlFragment
  | Docx
  | Pdf
  override x.ToString() =
    match x with
    | HtmlDocument -> "HtmlDocument"
    | HtmlFragment -> "HtmlFragment"
    | Docx -> "Docx"
    | Pdf -> "Pdf"

type KnowledgeBaseProcessor =
  | MarkdownConvertor of MarkdownConvertor

type Tool =
  | KnowledgeBaseProcessor of KnowledgeBaseProcessor
  | SemanticExtractor of SemanticExtractor
  with static member toUri =
    let inline uri x = Uri.from (sprintf "compilation:%s" (string x))
    function
    | KnowledgeBaseProcessor x -> uri x
    | SemanticExtractor x -> uri x

type Expression =
  | Expression of System.Text.RegularExpressions.Regex

type DirectoryPattern =
  { Id : Uri
    Expression : Expression }

type FilePattern =
  { Id : Uri
    Expression : Expression
    Tools : Tool list
    Represents : Uri }

type Capture = string * string

type ExpressionMatch =
  | Matches of Capture list

type ResourcePath =
  | ResourcePath of DirectoryPattern list * FilePattern

type ToolMatch =
  { Target : Target
    Represents : Uri
    Tools : Tool list
    Captured : Capture list }
  override x.ToString() =
    sprintf "%s is %s compiled by %A with %A" (string x.Target.Path)
      (string x.Represents) (x.Tools) (x.Captured)

type ToolOutput =
  { Provenence : Statement list
    Extracted : Resource list }

type ToolExecution =
  | Failure of ToolOutput
  | Success of ToolOutput

type PipelineStep =
  | PipelineStep of (ToolMatch * ToolExecution list)

type PipelineExecution =
  | Failure of Target * ToolOutput
  | Success of Target * ToolOutput

module pipeline =

  let prov = function
    | Failure(t,{Provenence=x;Extracted=_})
    | Success(t,{Provenence=x;Extracted=_}) -> Assertion.rdf.resource t.Id x

  let extracted = function
    | Failure(_,{Provenence=_;Extracted=x})
    | Success(_,{Provenence=_;Extracted=x}) -> x


  let succeed p e =
    ToolExecution.Success({ Provenence = p
                            Extracted = e })

  let fail p =
    ToolExecution.Failure({ Provenence = p
                            Extracted = [] })

  //Make a useful result type from a sequence of pipeline steps
  //Actually this is horrible, need to generalise success and failure before it multiplies out of control
  let result (PipelineStep(m, xe : ToolExecution list)) =
    let output =
      function
      | ToolExecution.Failure x -> x
      | ToolExecution.Success x -> x

    let output = List.map output xe

    let r =
      { Provenence =
          List.collect (fun { Provenence = p; Extracted = _ } -> p) output
        Extracted =
          List.collect (fun { Provenence = _; Extracted = e } -> e) output }
    match List.exists (function
            | ToolExecution.Success _ -> true
            | _ -> false) xe with
    | true -> PipelineExecution.Success(m.Target, r)
    | false -> PipelineExecution.Failure(m.Target, r)

module compilation =

  let mutable loader = System.IO.File.ReadAllText

  let matchesExpression (s, g) =
    seq {
      match g, s with
      | _, Segment "*" -> yield Some (Matches [])
      | Expression re, Segment s ->
        match re.Match s with
        | m when m.Length <> 0 ->
          yield Some
                  (Matches
                     [ for g in re.GetGroupNames() |> Seq.filter ((<>) "0") ->
                         (g, m.Groups.[g].Value) ])
        | _ -> yield None
    }

  let globs rp =
    seq {
      match rp with
      | ResourcePath(dx, fp) ->
        for d in dx -> d.Expression
        yield fp.Expression
    }

  let capture =
    function
    | Some(Matches cx) -> cx
    | None -> []

  let toolsFor t rp =
    match rp, t.Path with
    | ResourcePath(dx, fp), Path px ->
      let mx =
        globs rp
        |> Seq.zip px
        |> Seq.collect matchesExpression
      match mx |> Seq.exists ((=) None) with
      | false ->
        Some { Target = t
               Represents = fp.Represents
               Tools = fp.Tools
               Captured =
                 mx
                 |> Seq.toList
                 |> List.collect capture }
      | _ -> None

  [<AutoOpen>]
  module compilationuris =
    let directoryPattern =
      Uri.from ("http://ld.nice.org.uk/ns/compilation#DirectoryPattern")
    let filePattern = Uri.from ("http://ld.nice.org.uk/ns/compilation#FilePattern")
    let tool = Uri.from ("http://ld.nice.org.uk/ns/compilation#tool")
    let root = Uri.from ("http://ld.nice.org.uk/ns/compilation#Root")
    let parent = Uri.from ("http://ld.nice.org.uk/ns/compilation#parent")
    let expression = Uri.from ("http://ld.nice.org.uk/ns/compilation#expression")
    let represents = Uri.from ("http://ld.nice.org.uk/ns/compilation#represents")
    let compilation = Uri.from ("http://ld.nice.org.uk/ns/compilation#Compilation")
    let content = Uri.from ("http://ld.nice.org.uk/ns/compilation#content")
    let path = Uri.from ("http://ld.nice.org.uk/ns/compilation#path")

  let uriNode (Sys u) (Graph g) = g.CreateUriNode(u)

  let fromPredicateObject p o (Graph g) =
    seq {
      for s in g.GetTriplesWithPredicateObject
                 (uriNode p (Graph g), uriNode o (Graph g)) do
        yield! g.GetTriplesWithSubject(s.Subject)
    }

  let loadMake g =
    let xf = fromType filePattern g
    let fragment (Uri.Sys u) = u.Fragment

    let getExpression =
      function
      | FunctionalDataProperty expression xsd.string ex -> Expression(Regex ex)
      | fp -> failwith (sprintf "%A expression is not a functional property" fp)

    let getRepresents =
      function
      | FunctionalProperty represents (O(Node.Uri u, _)) -> u
      | fp -> failwith (sprintf "%A represents " fp)

    let getParent =
      function
      | TraverseFunctional parent p -> p
      | fp -> failwith (sprintf "%A has no parent" fp)

    //We should start inferring the compilation ontology so it's possible to
    //specify dependencies with inherited properties
    //For now we simply sort SemanticExtractors before KnowledgeBaseProcessors
    let orderTools =
      List.sortWith (fun x x' ->
        match x, x' with
        | SemanticExtractor(Content), SemanticExtractor _ -> -1
        | SemanticExtractor _, KnowledgeBaseProcessor _ -> -1
        | KnowledgeBaseProcessor _, SemanticExtractor _ -> 1
        | _ -> 0)

    let getTools =
      function
      | Property tool tx ->
        [ for (O(Uri u, _)) in tx do
            match fragment u with
            | "#Content" -> yield (SemanticExtractor Content)
            | "#YamlMetadata" -> yield (SemanticExtractor YamlMetadata)
            | "#HtmlDocument" ->
              yield (KnowledgeBaseProcessor(MarkdownConvertor HtmlDocument))
            | "#HtmlFragment" ->
              yield (KnowledgeBaseProcessor(MarkdownConvertor HtmlFragment))
            | "#Docx" -> yield (KnowledgeBaseProcessor(MarkdownConvertor(Docx)))
            | "#Pdf" -> yield (KnowledgeBaseProcessor(MarkdownConvertor(Pdf)))
            | _ -> () ]
      | tp -> failwithf "%A has no configured tools" tp

    let getFilePattern f =
      { Id = resourceId f
        Expression = getExpression f
        Tools = getTools f |> orderTools
        Represents = getRepresents f }

    let rec getDirectoryPath d : DirectoryPattern list =
      [ match d with
        | FunctionalProperty parent (O(Node.Uri r, _)) when r <> root ->
          yield! getDirectoryPath (getParent d)
        | _ -> ()
        yield { Id = resourceId d
                Expression = getExpression d } ]

    [ for f in xf ->
        ResourcePath(getDirectoryPath (getParent f), getFilePattern f) ]

  let scheme (Uri.Sys u) = u.Scheme
  let uripath (Uri.Sys u) = u.PathAndQuery

  let loadProvenance g =
    let uses = prefixes.prov + "uses" |> Uri.from
    let commit = prefixes.prov + "uses" |> Uri.from
    let specialisationOf = prefixes.prov + "specializationOf" |> Uri.from
    let informedBy = prefixes.prov + "informedBy" |> Uri.from
    let startedAtTime = prefixes.prov + "startedAtTime" |> Uri.from
    let id (R(S u, _)) = u

    let getEndedAt =
      function
      | FunctionalDataProperty startedAtTime xsd.string d -> d
      | r -> failwith (sprintf "%A has no startedAtTime property" r)

    let rec getCommits x =
      [ match x with
        | TraverseFunctional informedBy x ->
          yield { Id = id x
                  When = getEndedAt x }
          yield! getCommits x
        | _ -> () ]

    let getContent =
      function
      | FunctionalObjectProperty content l  ->
        match (scheme l) with
        | "http" | "https" -> FSharp.Data.Http.RequestString (string l)
        | "file" -> loader (uripath l)
        | _ -> failwithf "Cannot load content from %s" (string l)
      | r -> failwithf "%A has no content property" r

    let getPath =
      function
      | FunctionalDataProperty path xsd.string s -> s
      | r -> failwith (sprintf "%A has no path property" r)

    let getSpecialisationOf =
      function
      | FunctionalProperty specialisationOf (O(Node.Uri s, _)) -> s
      | r -> failwith (sprintf "%A has no specialisationOf" r)

    let getUses =
      function
      | Traverse uses xe ->
        [ for e in xe ->
            { Id = getSpecialisationOf e
              ProvId = id e
              Content = getContent e
              Path = getPath e |> toPath } ]
      | _ -> []

    match fromType compilation g with
    | [] -> failwith "Input contains no compilation resource"
    | c :: _ ->
      { Id = id c
        Commits = getCommits c
        Targets = getUses c }

module Tracing =
  open Assertion
  open rdf

  type Location =
    | File of (Path * int option * int option)
    | Resource of Uri

  let fileLocation p = File(p, None, None)
  let lineLocation p l = File(p, Some l, None)
  let charlocation p l c = File(p, Some l, Some c)
  let resourceLocation r = Resource(resourceId r)

  let private ifSome f x =
    [ match x with
      | Some x -> yield f x
      | _ -> () ]

  let private position =
    function
    | (File(Path p, line, char)) ->
      List.concat
        [ [ a !"compilation:InFile"
            dataProperty !"compilation:path" ((string p) ^^ xsd.string) ]

          ifSome
            (fun l ->
            dataProperty !"compilation:charPosition" ((string l) ^^ xsd.string))
            char

          ifSome
            (fun l ->
            dataProperty !"compilation:charPosition" ((string l) ^^ xsd.string))
            char ]
    | Resource uri ->
      [ a !"compilation:FromResource"
        objectProperty !"compilation:source" uri ]

  let message t s p =
    blank !"compilation:message" [ a t
                                   dataProperty !"rdfs:label" (s ^^ xsd.string)
                                   blank !"compilation:position" (position p) ]

  let warn = message !"compilation:Warning"
  let info = message !"compilation:Information"
  let error = message !"compilation:Error"
