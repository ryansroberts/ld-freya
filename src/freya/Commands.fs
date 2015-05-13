namespace Freya
module Commands =
open FSharp.RDF
open Freya.compilation
open Assertion
type Command =
  | Describe of Path


let descriptionOf = function
  | ResourcePath (_,{
    Id = id
    Expression = e
    Tools = xt
    Represents = rep
    }) -> [
    rdf.resource
    ]

let describe xr (Path xs) =
  xr |> Seq.map
     (fun r ->
      let m = globs r
              |> Seq.zip xs
              |> Seq.collect matchesExpression
      match (Seq.exists ((=) None) m) with
      | true -> None
      | false -> Some r
      )
     |> Seq.filter Option.isSome
     |> Seq.map Option.get
     |> Seq.collect descriptionOf

let exec xr = function
  | Describe p -> describe xr p
