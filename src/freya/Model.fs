module Model

open common.RDF
open VDS.RDF
open FSharpx
open System.Text.RegularExpressions

type Segment =
  | Segment of string

type Path = 
    | Path of Segment list
  with static member fromStr s =
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
  
type Directory = {
    Id : Uri
    Parent : Directory option
    Glob : Glob
 }

type FilePattern = {
    Id : Uri
    Parent : Directory
    Glob : Glob
    Tools : Tool list
    Represents : Uri
    }

type Capture = string * string

type GlobMatch =
  | Matches of Capture list
  | None

type ResourcePath =
  | ResourcePath of (Directory list * FilePattern)

let globs rp =
  match rp with
    | ResourcePath (dx,fp) -> [
        for d in dx -> d.Glob
        yield fp.Glob
      ]

let matchesGlob ( g,s ) = [
  match g,s with
  | Glob re,Segment s ->
  match re.Match s with
  | m when m.Length <> 0 -> yield Matches []
  | _ -> ()
  ]

let matchesTarget rp p =
  match rp,p with
    | rp,Path px ->
      List.zip (globs rp) px
      |> List.collect matchesGlob
      
  
