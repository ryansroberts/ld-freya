module Model

open common.RDF
open VDS.RDF
open FSharpx
open System.Text.RegularExpressions

type Segment =
    | Segment of string

type Path =
    | Path of Segment list
    static member fromStr s =
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
    | EchoContent

type Glob =
    | Glob of System.Text.RegularExpressions.Regex

type DirectoryPattern =
    { Id : Uri
      Parent : DirectoryPattern option
      Glob : Glob }

type FilePattern =
    { Id : Uri
      Parent : DirectoryPattern
      Glob : Glob
      Tools : Tool list
      Represents : Uri }

type Capture = string * string

type GlobMatch =
    | Matches of Capture list

type ResourcePath =
    | ResourcePath of (DirectoryPattern list * FilePattern)

type ToolMatch = {
  Target : Target
  Tools : Tool list
  Captured: Capture list
}

let matchesGlob (s, g) = seq {
     match g, s with
      | Glob re, Segment s ->
          match re.Match s with
          | m when m.Length <> 0 -> yield Some (Matches [])
          | _ -> yield None }

let globs rp = seq {
  match rp with
  | ResourcePath ( dx,fp ) ->
  for d in dx -> d.Glob
  yield fp.Glob
  }

let capture = function
  | Some (Matches cx) -> cx
  | None -> []

let toolsFor rp t =
    match rp,t.Path with
      | ResourcePath (dx,fp),Path px ->
        let mx = globs rp
                 |> Seq.zip px
                 |> Seq.collect matchesGlob
        match mx |> Seq.exists ((=) None) with
          | false -> Some {
            Target = t
            Tools = fp.Tools
            Captured = mx |> Seq.toList |> List.collect capture}
          | _ -> None

let loadMake s =
  let roots = Store.resultset s """
    select ?root
    where {
      ?root a compilation:Directory .
    }
  """


