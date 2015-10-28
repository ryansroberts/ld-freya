namespace Freya

open System.Text.RegularExpressions
open FSharp.RDF
open resource
open ExtCore

[<AutoOpen>]
module prelude =
  let inline (|?) (a : 'a option) b =
    if a.IsSome then a.Value
    else b

type Segment =
  | Segment of string
  override x.ToString() =
    let (Segment s) = x
    s

type SysPath = System.IO.Path

type SysFile = System.IO.File

type Path =
  | Path of Segment list
  | AbsolutePath of Segment list

  static member (++) (a, b) =
    match a, b with
    | AbsolutePath a, Path b | AbsolutePath a, AbsolutePath b ->
      AbsolutePath(a @ b)
    | Path a, AbsolutePath b | Path a, Path b -> Path(a @ b)

  static member (++) (p, f) = File(p, f)

  override x.ToString() =
    match x with
    | Path xs -> System.String.Join("/", xs)
    | AbsolutePath xs -> "/" + System.String.Join("/", xs)

  static member segments = function
    | Path xs | AbsolutePath xs -> xs

  static member from s =
    let segments = String.split [| '/' |] s
    let combine = Seq.map Segment >> List.ofSeq
    if SysPath.IsPathRooted s then combine (Seq.skip 1 segments) |> AbsolutePath
    else combine segments |> Path

  static member ensurePathExists (File(p, f)) =
    let d = System.IO.DirectoryInfo(string p)
    d.Create()
    File(p, f)

and FileName = string

and Extension = string

and FullName =
  | FullName of (FileName * Extension)

and File =
  | File of Path * FullName

  override x.ToString() =
    match x with
    | File(p, FullName(f, e)) -> sprintf "%s/%s%s" (string p) f e

  static member exists (f : File) = System.IO.File.Exists(string f)
  static member from s =
    File
      (SysPath.GetDirectoryName s |> Path.from,
       FullName(SysPath.GetFileNameWithoutExtension s, SysPath.GetExtension s))
  static member path (File(x, _)) = x
  static member fullname (File(_, x)) = x
  static member name (File(_, (FullName(x, _)))) = x
  static member write s f = SysFile.WriteAllText(string f, s)

type Target =
  { Id : Uri
    Specialisation : Uri
    Commit : Uri
    Compilation : Uri
    Path : File
    Content : Uri * string }

type Commit =
  { Id : Uri
    When : string }

type Provenance =
  { Id : Uri
    Commits : Commit seq
    Targets : Target seq
    InformedBy : Uri seq }

type Mime =
  | Docx
  | Pdf
  | HtmlDocument
  | HtmlFragment

  static member name =
    function
    | Docx -> "Docx"
    | Pdf -> "Pdf"
    | HtmlDocument -> "HtmlDocument"
    | HtmlFragment -> "HtmlFragment"

  static member toUri x =
    let mimeUri = sprintf "http://purl.org/NET/mediatypes/%s" >> Uri.from
    match x with
    | Docx ->
      mimeUri
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    | HtmlDocument | HtmlFragment -> mimeUri "text/html"
    | Pdf -> mimeUri "application/pdf"

and Name =
  | Name of string

and [<CustomEquality; NoComparison>] ToolFn =
  | ToolFn of (ToolMatch -> Resource list -> ToolExecution)

  override x.Equals(y) =
    match y with
    | :? ToolFn -> true
    | _ -> false

  override x.GetHashCode() = 0

and Tool =
  | Pandoc of Mime
  | SemanticExtractor of Name * ToolFn

  static member toUri =
    Tool.name
    >> sprintf "compilation:%s"
    >> Uri.from

  static member name =
    function
    | Pandoc x -> Mime.name x
    | SemanticExtractor(Name x, _) -> x

and Expression =
  | Seq of Expression list
  | Wildcard of string
  | Literal of string
  | Variable of string

and DirectoryPattern =
  { Id : Uri
    Expression : Expression }

and FilePattern =
  { Id : Uri
    Expression : Expression
    Tools : Tool list
    Template : string option
    Represents : Uri }

and Capture = string * string

and ExpressionMatch =
  | Matches of Capture list

and ResourcePath =
  | ResourcePath of DirectoryPattern list * FilePattern

and ToolMatch =
  { Target : Target
    Represents : Uri
    Tools : Tool list
    Captured : Capture list }
  override x.ToString() =
    sprintf "%s is %s compiled by %A with %A" (string x.Target.Path)
      (string x.Represents) (x.Tools) (x.Captured)

and ToolOutput =
  { Provenance : Resource list //Blank nodes on compilation * new prov entries
    Extracted : Resource list }

and ToolExecution =
  | Failure of ToolOutput
  | Success of ToolOutput

type PipelineStep =
  | PipelineStep of (ToolMatch * ToolExecution list)

type PipelineExecution =
  | Failure of Target * ToolOutput
  | Success of Target * ToolOutput


module pipeline =
  let prov =
    function
    | Failure(t, { Provenance = x; Extracted = _ }) | Success(t,
                                                              { Provenance = x;
                                                                Extracted = _ }) ->
      x
  let extracted =
    function
    | Failure(_, { Provenance = _; Extracted = x }) | Success(_,
                                                              { Provenance = _;
                                                                Extracted = x }) ->
      x

  let succeed p e =
    ToolExecution.Success({ Provenance = p
                            Extracted = e })

  let fail p =
    ToolExecution.Failure({ Provenance = p
                            Extracted = [] })

  //Make a useful result type from a sequence of pipeline steps
  //Actually this is horrible, need to get some either stuff going on before it multiplies out of control
  let result (PipelineStep(m, xe : ToolExecution list)) =
    let output =
      function
      | ToolExecution.Failure x -> x
      | ToolExecution.Success x -> x

    let output = List.map output xe

    let r =
      { Provenance =
          List.collect (fun { Provenance = p; Extracted = _ } -> p) output
        Extracted =
          List.collect (fun { Provenance = _; Extracted = e } -> e) output }
    match List.exists (function
            | ToolExecution.Success _ -> true
            | _ -> false) xe with
    | true -> PipelineExecution.Success(m.Target, r)
    | false -> PipelineExecution.Failure(m.Target, r)

[<AutoOpen>]
module expression =
  type Uri with

    static member fragment u =
      let u = (Uri.toSys u)
      u.Fragment.Substring(1, u.Fragment.Length - 1)

    static member scheme u = (Uri.toSys u).Scheme
    static member uripath u = (Uri.toSys u).PathAndQuery

  open FParsec

  let private str = pstring

  let private pLiteral<'a> =
    many1Chars (choice [ letter
                         digit
                         pchar '.'
                         pchar '-'
                         pchar '_' ])
    |>> Literal

  let private pVariable<'a> =
    between (str "$(") (str ")") ((many1Chars letter) |>> Variable)
  let private pWildcard<'a> = str "*" |>> Wildcard
  let private pExpressionC = choice [ pWildcard; pVariable; pLiteral ]
  let private pSeq = many pExpressionC |>> Seq
  let private pExpression = pSeq .>> eof

  let private runParser str p =
    match run p str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwithf "Failed to parse %s" errorMsg

  type Expression with
    static member parse s = runParser s pExpression

    static member matcher r =
      let rec matcher =
        function
        | Literal s -> s
        | Wildcard _ -> ".*"
        | Variable v -> (sprintf "(?<%s>.*)" v)
        | Seq xs ->
          xs
          |> List.map matcher
          |> String.concat ""

      let re = Regex(matcher r)
      (fun s ->
      match re.Match s with
      | m when m.Length <> 0 ->
        Some
          (Matches
             [ for g in re.GetGroupNames() |> Seq.filter ((<>) "0") ->
                 (g, m.Groups.[g].Value) ])
      | _ -> None)

    static member reifier xs r =
      let rec reifier =
        function
        | Literal s -> s
        | Wildcard _ -> "*"
        | Variable v ->
          xs
          |> List.find (snd >> ((=) v))
          |> snd
        | Seq xs ->
          xs
          |> List.map reifier
          |> String.concat ""
      reifier r

[<AutoOpen>]
module compilation =
  let mutable loader = System.IO.File.ReadAllText

  let matchesExpression (s, g) =
    seq {
      match g, s with
      | _, Segment "*" -> yield Some(Matches [])
      | ex, Segment s -> yield Expression.matcher ex s
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

  type Target with
    static member toolsFor t rp =
      match rp, t.Path with
      | ResourcePath(dx, fp), (File(p, FullName(f, ex))) ->
        let mx =
          globs rp
          |> Seq.zip ((Path.segments p) @ [ (Segment(f + ex)) ])
          |> Seq.collect matchesExpression
        if mx
           |> Seq.exists ((=) None)
           |> not
        then
          printfn "Executing %A against target %s" fp.Tools (string t.Path)
          Some { Target = t
                 Represents = fp.Represents
                 Tools = fp.Tools
                 Captured =
                   mx
                   |> Seq.toList
                   |> List.collect capture }
        else None

[<AutoOpen>]
module compilationuris =
  let template = Uri.from ("http://ld.nice.org.uk/ns/compilation#template")
  let represents = Uri.from ("http://ld.nice.org.uk/ns/compilation#represents")
  let compilation =
    Uri.from ("http://ld.nice.org.uk/ns/compilation#Compilation")
  let content = Uri.from ("http://ld.nice.org.uk/ns/compilation#content")
  let path = Uri.from ("http://ld.nice.org.uk/ns/compilation#path")
  let uriNode u (Graph g) = g.CreateUriNode(Uri.toSys u)

  let fromPredicateObject p o (Graph g) =
    seq {
      for s in g.GetTriplesWithPredicateObject
                 (uriNode p (Graph g), uriNode o (Graph g)) do
        yield! g.GetTriplesWithSubject(s.Subject)
    }


  let loadProvenance g =
    let g =
      g
      |> Graph.addPrefixes (Uri.from "http://ld.nice.org.uk/prov")
           [ ("prov", Uri.from "http://www.w3.org/ns/prov#") ]
    let uses = "http://www.w3.org/ns/prov#uses" |> Uri.from
    let specialisationOf =
      "http://www.w3.org/ns/prov#specializationOf" |> Uri.from
    let informedBy = "http://www.w3.org/ns/prov#informedBy" |> Uri.from
    let wasGeneratedBy = "http://www.w3.org/ns/prov#wasGeneratedBy" |> Uri.from
    let startedAtTime = "http://www.w3.org/ns/prov#startedAtTime" |> Uri.from
    let id (R(S u, _)) = u

    let getEndedAt =
      function
      | FunctionalDataProperty startedAtTime xsd.string d -> d
      | r -> failwith (sprintf "%A has no startedAtTime property" r)

    let rec getCommits x =
      seq {
        match x with
        | TraverseFunctional informedBy x ->
          yield { Id = Resource.id x
                  When = getEndedAt x }
          yield! getCommits x
        | _ -> ()
      }

    let getContent =
      function
      | FunctionalObjectProperty content l ->
        match (Uri.scheme l) with
        | "http" | "https" -> (l, FSharp.Data.Http.RequestString(string l))
        | "file" -> (l, loader (Uri.uripath l))
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

    let getInformedBy =
      function
      | ObjectProperty informedBy x -> x
      | r -> []

    let getWasGeneratedBy =
      function
      | FunctionalObjectProperty wasGeneratedBy x -> x
      | r -> failwith (sprintf "%A has no wasGeneratedBy" r)

    let getUses r =
      match r with
      | Traverse uses xe ->
        seq {
          for e in xe ->
            { Id = id e
              Specialisation = getSpecialisationOf e
              Commit = getWasGeneratedBy e
              Compilation = Resource.id r
              Content = getContent e
              Path = getPath e |> File.from }
        }
      | _ -> Seq.empty

    match Resource.fromType compilation g with
    | [] -> failwith "Input contains no compilation resource"
    | c :: _ ->
      { Id = id c
        Commits = getCommits c
        Targets = getUses c
        InformedBy = getInformedBy c }

module Tracing =
  open Assertion
  open rdf

  type Location =
    | FileLocation of (File * int option * int option)
    | Resource of Uri
    override x.ToString() =
      match x with
      | FileLocation(f, l, c) -> sprintf "%s" (string f)
      | Resource r -> (string r)

  let fileLocation f = FileLocation(f, None, None)
  let lineLocation p l = FileLocation(p, Some l, None)
  let charlocation p l c = FileLocation(p, Some l, Some c)
  let resourceLocation r = Resource(Resource.id r)

  let private ifSome f x =
    [ match x with
      | Some x -> yield f x
      | _ -> () ]

  let private position =
    function
    | (FileLocation(f, line, char)) ->
      List.concat
        [ [ a !!"compilation:InFile"
            dataProperty !!"compilation:path" ((string f) ^^ xsd.string) ]

          ifSome
            (fun l ->
            dataProperty !!"compilation:charPosition" ((string l) ^^ xsd.string))
            char

          ifSome
            (fun l ->
            dataProperty !!"compilation:charPosition" ((string l) ^^ xsd.string))
            char ]
    | Resource uri ->
      [ a !!"compilation:FromResource"
        objectProperty !!"compilation:source" uri ]

  let message t s p =
    printfn """{"type" : "%s","location" : "%s","message" : "%s"}""" (string t) (string p) s
    blank !!"compilation:message"
      [ a t
        dataProperty !!"rdfs:label" (s ^^ xsd.string)
        blank !!"compilation:position" (position p) ]

  let warn = message !!"compilation:Warning"
  let info = message !!"compilation:Information"
  let error = message !!"compilation:Error"

  open System

  /// <summary>
  /// Emit prov generation and deriviation.
  /// </summary>
  let toolProv tm id genId mime xs =
    let derived =
      rdf.resource id ([ a !!"http://www.w3.org/ns/prov#Entity"

                         objectProperty !!"compilation:wasDerivedFrom"
                           (fst tm.Target.Content)

                         blank !!"http://www.w3.org/ns/prov#qualifiedDeriviation"
                           [ a !!"http://www.w3.org/ns/prov#Deriviation"

                             objectProperty !!"http://www.w3.org/ns/prov#entity"
                               (fst tm.Target.Content)

                             objectProperty
                               !!"http://www.w3.org/ns/prov#hadGeneration" genId ] ]
                       @ (Option.toList mime))

    let generation =
      rdf.resource genId ([ a !!"http://www.w3.org/ns/prov#Generation"
                            a !!"http://www.w3.org/ns/prov#InstantaneousEvent"

                            objectProperty
                              !!"http://www.w3.org/ns/prov#wasGeneratedBy"
                              tm.Target.Compilation

                            blank
                              !!"http://www.w3.org/ns/prov#qualifiedGeneration"
                              [ a !!"http://www.w3.org/ns/prov#Generation"

                                a
                                  !!"http://www.w3.org/ns/prov#InstantaneousEvent"

                                dataProperty
                                  !!"http://www.w3.org/ns/prov#atTime"
                                  (DateTimeOffset.Now ^^ xsd.datetime)

                                objectProperty
                                  !!"http://www.w3.org/ns/prov#activity"
                                  tm.Target.Compilation ] ]
                          @ xs)

    [ derived; generation ]

  let alternateRepresentation tm id =
    [ rdf.resource id
        [ objectProperty !!"http://www.w3.org/ns/prov#alternateOf" tm.Target.Id ] ]
  let semanticExtraction tm (Name n) =
    toolProv tm tm.Target.Id
      (Uri.from (sprintf "%s-%s" (string tm.Target.Id) n)) None

  let generatedResource tm id mime xs =
    let genId =
      (Uri.from (sprintf "%s-%s" (string tm.Target.Id) (Mime.name mime)))
    let format = Some(objectProperty !!"dcterms:format" (Mime.toUri mime))
    toolProv tm id genId format xs @ alternateRepresentation tm id
