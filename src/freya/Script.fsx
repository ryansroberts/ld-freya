#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharpx.Core/lib/40/FSharpx.Core.dll"
#r "../../packages/FSharp.RDF/lib/net40/FSharp.RDF.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#load "Model.fs"
#load "Tools.fs"
open Freya
open System.Text.RegularExpressions
open FSharp.RDF
open Swensen.Unquote
open compilation

let matchingTarget =
    { Id = Uri.from "http://nice.org.uk/ns/target1"
      ProvId = Uri.from "http://nice.org.uk/qualitystandards/resource"
      Path = Path.from "qualitystandards/standard_1/statement_23.md"
      Content = "" }

let nonMatchingTarget =
    { Id = Uri.from "http://nice.org.uk/ns/target1"
      ProvId = Uri.from "http://nice.org.uk/qualitystandards/resource"
      Path = Path.from "qualitystandards/lol/standard_23.md"
      Content = "" }

let prov = """@base <http://nice.org.uk/ns/compilation#>.

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.
@prefix prov: <http://www.w3.org/ns/prov#>.
@prefix owl: <http://www.w3.org/2002/07/owl#>.
@prefix compilation: <http://nice.org.uk/ns/compilation#>.
@prefix cnt: <http://www.w3.org/2011/content#>.
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

<http://nice.org.uk/ns/prov/commit#999586c1dfe8a71c6cbf6c129f404c5642ff31bd>
  a prov:Commit;
  compilation:path "qualitystandards/standard_1/statement_23.md";
  cnt:chars "Some content"^^xsd:string;
  prov:specializationOf <http://nice.org.uk/ns/prov/new.md>;
  prov:wasAttributedTo <http://nice.org.uk/ns/prov/user/schacon@gmail.com>;
  prov:wasGeneratedBy <http://nice.org.uk/ns/prov/commit/c47800c>.

<http://nice.org.uk/ns/prov/commit#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd>
  a prov:Commit;
  prov:informedBy <http://nice.org.uk/ns/prov/commit#999586c1dfe8a71c6cbf6c129f404c5642ff31bd> ;
  compilation:path "qualitystandards/standard_1/statement_23.md";
  cnt:chars "Some content"^^xsd:string;
  prov:specializationOf <http://nice.org.uk/ns/prov/new.md>;
  prov:wasAttributedTo <http://nice.org.uk/ns/prov/user/schacon@gmail.com>;
  prov:wasGeneratedBy <http://nice.org.uk/ns/prov/commit/c47800c>.

<http://nice.org.uk/ns/compilation#compilation_2015-02-23T12:12:47.2583040+00:00>
  a compilation:Compilation;
  rdfs:label "Change this to a compilation message that is actualy useful to somebody";
  prov:informedBy <http://nice.org.uk/ns/prov/commit#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd>;
  prov:qualifiedAssociation [a prov:Association ;
                             prov:agent <http://nice.org.uk/ns/prov#user/ryanroberts> ;
                             prov:hadRole "initiator"];
  prov:startedAtTime "2015-02-23T12:12:47.259270+00:00"^^xsd:dateTime;
  prov:uses <http://nice.org.uk/ns/prov/entity#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd/new.md>;
  prov:wasAssociatedWith <http://nice.org.uk/ns/prov/user/ryanroberts>.

<http://nice.org.uk/ns/prov/entity#a71586c1dfe8a71c6cbf6c129f404c5642ff31bd/new.md>
  a prov:Entity;
  compilation:path "qualitystandards/standard_1/statement_23.md";
  cnt:chars "Some content"^^xsd:string;
  prov:specializationOf <http://nice.org.uk/ns/prov/new.md>;
  prov:wasAttributedTo <http://nice.org.uk/ns/prov/user/schacon@gmail.com>;
  prov:wasGeneratedBy <http://nice.org.uk/ns/prov/commit/c47800c>.
"""
open FSharp.RDF
open resource
let fromType u g = fromObject u g |> List.collect (function
                                       | R(S s, _) -> fromSubject s g)


let loadProvenance g  =
  let uses = prefixes.prov + "uses" |> Uri.from
  let commit = prefixes.prov + "uses" |> Uri.from
  let specialisationOf = prefixes.prov + "specializationOf" |> Uri.from
  let informedBy = prefixes.prov + "informedBy" |> Uri.from
  let startedAtTime = prefixes.prov + "startedAtTime" |> Uri.from;
  let chars = prefixes.cnt + "chars" |> Uri.from
  let path = prefixes.compilation + "path" |> Uri.from
  let id (R(S u, _)) = u
  let compilation = Uri.from (prefixes.compilation + "Compilation")

  let getEndedAt = function
      | FunctionalDataProperty startedAtTime xsd.datetimeoffset d -> d
      | r -> failwith(sprintf "%A has no endedAtTime property" r)

  let rec getCommits x = [
      match x with
      | TraverseFunctional informedBy x ->
        printfn "%A" x
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


let provM = graph.loadFormat graph.parse.ttl (graph.fromString prov) |> loadProvenance

