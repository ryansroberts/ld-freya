namespace Freya
module Commands =
open FSharp.RDF
open Freya.compilation
open Assertion
open rdf
open Freya
type Command =
  | Describe of Path




let descriptionOf rp =
  match rp with
  | ResourcePath (xr,{
    Id = id
    Expression = e
    Tools = xt
    Represents = rep
    }) as rp -> [
        resource !("http://ld.nice.org.uk/command") ([
            a !"http://ld.nice.org.uk/ns/compilation/Command"
            objectProperty !"compilation:represents" rep
        ] @ List.map (fun t -> objectProperty !"compilation:tool" (Tool.toUri t)) xt)
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
