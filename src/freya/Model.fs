module Model

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

type Target =
  { Id : Uri
    Path : Path
    Content : string }

type Compilation =
  { Id : Uri
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
    Tools : Tool list
    Captured : Capture list }

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

let toolsFor rp t =
  match rp, t.Path with
  | ResourcePath(dx, fp), Path px ->
    let mx =
      globs rp
      |> Seq.zip px
      |> Seq.collect matchesExpression
    match mx |> Seq.exists ((=) None) with
    | false ->
      Some { Target = t
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
    | FunctionalProperty represents (O(Uri u)) -> u
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
      | FunctionalProperty parent (O(Uri r)) when r <> root ->
        yield! getDirectoryPath (getParent d)
      | _ -> ()
      yield { Id = id d
              Expression = getExpression d } ]

  [ for f in xf ->
      ResourcePath(getDirectoryPath (getParent f), getFilePattern f) ]

let loadCompilation g : Compilation =
  let uses = prefixes.prov + "uses" |> Uri.from
  let chars = prefixes.cnt + "chars" |> Uri.from
  let path = prefixes.compilation + "path" |> Uri.from
  let id (R(S u, _)) = u

  let getChars =
    function
    | FunctionalDataProperty chars xsd.string s -> s
    | r -> failwith (sprintf "%A has no content property" r)

  let getPath =
    function
    | FunctionalDataProperty path xsd.string s -> s
    | r -> failwith (sprintf "%A has no path property" r)

  let getUses = function
    | Traverse uses xe ->
      [ for e in xe ->
          { Id = id e
            Content = getChars e
            Path = getPath e |> Path.from } ]
  match fromType compilation g with
  | [] -> failwith "Input contains no compilation resource"
  | c :: _ ->
    { Id = id c
      Targets = getUses c }
