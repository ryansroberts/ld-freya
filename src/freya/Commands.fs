namespace Freya

module Commands = 
  open FSharp.RDF
  open Freya.compilation
  open Assertion
  open rdf
  open Freya
  
  type Command = 
    | Describe of Path
    | CreateResource of Uri * (string * string) list
  
  let descriptionOf rp = 
    let postFix u p = Uri.from (sprintf "%s:%s" (string u) p)
    match rp with
    | ResourcePath(xr, { Id = id; Expression = e; Tools = xt; Represents = rep }) as rp -> 
      [ resource (postFix rep "description") 
          ([ a !"http://ld.nice.org.uk/ns/compilation/Description"
             objectProperty !"compilation:represents" rep
             objectProperty !"compilation:action" (postFix rep "CreateResource") ]
           @ List.map 
               (fun t -> objectProperty !"compilation:tool" (Tool.toUri t)) xt) ]
  
  let describe xr (Path xs) = 
    xr
    |> Seq.map (fun r -> 
         let m = 
           globs r
           |> Seq.zip xs
           |> Seq.collect matchesExpression
         match (Seq.exists ((=) None) m) with
         | true -> None
         | false -> Some r)
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.collect descriptionOf
    |> List.ofSeq
  
  let createResource xr (uri, xp) = 
    let reifyRp rp = 
      globs rp
      |> Seq.map (Expression.reifier xp)
      |> String.concat "/"
      |> File.from
    
    let hasRepresentation (ResourcePath(xs, r)) = 
      if r.Represents = uri then Some(ResourcePath(xs, r))
      else None
    
    match xr |> Seq.tryPick hasRepresentation with
    | Some(ResourcePath(xd, fp)) -> 
      reifyRp (ResourcePath(xd, fp)) |> File.write (fp.Template |? "")
      [ resource uri [] ]
    | None -> [ resource uri [] ]
  
  let exec xr = 
    function 
    | Describe p -> describe xr p
    | CreateResource(uri, xp) -> createResource xr (uri, xp)
