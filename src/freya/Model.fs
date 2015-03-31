namespace Freya


open VDS.RDF
open FSharpx
open System.Text.RegularExpressions
open FSharp.RDF
open resource

type Segment =
  | Segment of string

type Path =
  | Path of Segment list
  static member from s =
    Strings.split '/' s
    |> Array.map Segment
    |> Array.toList
    |> Path.Path
  override x.ToString() =
    match x with
    | Path xs -> System.String.Join("/",xs)

[<AutoOpen>]
module path =
  let (++) (Path a) (Path b) = a @ b


type Target =
  { Id : Uri
    ProvId : Uri
    Path : Path
    Content : string }


type Commit =
  { Id : Uri
    When: System.DateTimeOffset}

type Provenance =
  { Id : Uri
    Commits : Commit list
    Targets : Target list }

type Tool =
  | Process of string * string
  | Content

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

open Store

type ToolFailure =
  { Prov : Resource list}
type ToolSuccess =
  { Prov : Resource list
    Output : Resource list}
type ToolExecution =
  | Failure of ToolFailure
  | Success of ToolSuccess


module compilation =

let matchesExpression (s, g) =
  seq {
    match g, s with
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
  let directoryPattern = Uri.from (prefixes.compilation + "DirectoryPattern")
  let filePattern = Uri.from (prefixes.compilation + "FilePattern")
  let tool = Uri.from (prefixes.compilation + "tool")
  let root = Uri.from (prefixes.compilation + "Root")
  let parent = Uri.from (prefixes.compilation + "parent")
  let expression = Uri.from (prefixes.compilation + "expression")
  let represents = Uri.from (prefixes.compilation + "represents")
  let compilation = Uri.from (prefixes.compilation + "Compilation")

let fromType u g = fromObject u g |> List.collect (function
                                       | R(S s, _) -> fromSubject s g)

let loadMake g =
  let xf = fromType filePattern g

  let getExpression =
    function
    | FunctionalDataProperty expression xsd.string ex -> Expression(Regex ex)
    | fp -> failwith (sprintf "%A expression is not a functional property" fp)

  let getRepresents =
    function
    | FunctionalProperty represents (O(Node.Uri u,_)) -> u
    | fp -> failwith (sprintf "%A represents " fp)

  let getParent =
    function
    | TraverseFunctional parent p -> p
    | fp -> failwith (sprintf "%A has no parent" fp)

  let id (R(S u, _)) = u

  let getFilePattern f =
    { Id = id f
      Expression = getExpression f
      Tools = [ Content ]
      Represents = getRepresents f }

  let rec getDirectoryPath d : DirectoryPattern list =
    [ match d with
      | FunctionalProperty parent (O(Node.Uri r,_)) when r <> root ->
        yield! getDirectoryPath (getParent d)
      | _ -> ()
      yield { Id = id d
              Expression = getExpression d } ]

  [ for f in xf -> ResourcePath(getDirectoryPath (getParent f), getFilePattern f) ]

let loadProvenance g  =
  let uses = prefixes.prov + "uses" |> Uri.from
  let commit = prefixes.prov + "uses" |> Uri.from
  let specialisationOf = prefixes.prov + "specializationOf" |> Uri.from
  let informedBy = prefixes.prov + "informedBy" |> Uri.from
  let endedAtTime = prefixes.prov + "endedAtTime" |> Uri.from;
  let chars = prefixes.cnt + "chars" |> Uri.from
  let path = prefixes.compilation + "path" |> Uri.from
  let id (R(S u, _)) = u

  let getEndedAt =
    function
      | FunctionalDataProperty endedAtTime xsd.datetimeoffset d -> d
      | r -> failwith(sprintf "%A has no endedAtTime property" r)

  let rec getCommits x = [
      match x with
      | TraverseFunctional informedBy x ->
        yield {
            Id = id x
            When = getEndedAt x
        }
      | _ -> ()
      ]
  let getChars =
    function
    | FunctionalDataProperty chars xsd.string s -> s
    | r -> failwith (sprintf "%A has no content property" r)

  let getPath =
    function
    | FunctionalDataProperty path xsd.string s -> s
    | r -> failwith (sprintf "%A has no path property" r)

  let getSpecialisationOf =
    function
    | FunctionalProperty specialisationOf (O(Node.Uri s,_)) -> s
    | r -> failwith (sprintf "%A has no specialisationOf" r)

  let getUses = function
    | Traverse uses xe ->
      [ for e in xe ->
          { Id = getSpecialisationOf e
            ProvId = id e
            Content = getChars e
            Path = getPath e |> Path.from } ]
  match fromType compilation g with
  | [] -> failwith "Input contains no compilation resource"
  | c :: _ ->
    { Id = id c
      Commits = getCommits c
      Targets = getUses c }
