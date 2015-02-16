module Model

open VDS.RDF

type Uri = 
  | Uri of string

type Path = 
  | Path of string

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

type Rule = Target -> Tool option
