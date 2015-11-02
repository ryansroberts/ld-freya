namespace Freya

open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Query
open Nessos.UnionArgParser
open System.IO
open ExtCore
open FSharp.Data
open FSharp.RDF
open FSharp.RDF.Store
open FSharp.RDF.JsonLD
open FSharp.Text.RegexProvider
open JsonLD.Core
open Newtonsoft.Json.Linq
open FSharp.Collections.ParallelSeq
type PathRegex = Regex< ".*<(?<firstPartOfPropertyPath>.*)>.*">


type Logger =
  | Debug
  | Info
  with member inline __.write level message =
    match __,level,message with
    | Info,_,s -> System.Console.WriteLine (string s);s
    | Debug,Debug,s -> System.Console.WriteLine (string s);s
    | _,_,s -> s

module Publication =
  type PropertyPaths =
    | PropertyPaths of Uri list list

  let publish (log:Logger) (stardog : Store) commit propertyPaths contexts =
    let contextS (uri) = sprintf """ "%s" """ uri
    let contexts = contexts |> List.map contextS
    let context = (JObject.Parse(sprintf """ {
                "@context": [
                     {"@language" : "en"},
                     %s,
                     {"resource" : "http://ld.nice.org.uk/resource#" }
                  ]
               }
    """ (String.concat ",\n" contexts)) :> JToken)

    let urinode =
      function
      | Node.Uri x -> Some x
      | _ -> None

    let asUri =
      Seq.map urinode
      >> Seq.filter Option.isSome
      >> Seq.map Option.get

    let resources =
      (stardog.queryResultSet [] """
            prefix prov:  <http://www.w3.org/ns/prov#>

            select distinct ?resource
            from <http://ld.nice.org.uk/ns>
            from <http://ld.nice.org.uk/prov>
            from <http://ld.nice.org.uk/>
            where {
            ?s prov:specializationOf ?resource .
            }
            """ []
       |> ResultSet.singles
       |> asUri)

    let rec retry f x =
        try
          f x
        with
          | e ->
            sprintf "Failure: %s" e.Message |> log.write Info |> ignore
            retry f x


    let entityForResource resource =
      (sprintf "Getting entity for resource %A" resource) |> log.write Debug |> ignore
      stardog.queryResultSet [] """
            prefix prov: <http://www.w3.org/ns/prov#>
            prefix niceprov: <http://ld.nice.org.uk/prov>
            prefix compilation: <http://ld.nice.org.uk/ns/compilation#>
            prefix nice: <http://ld.nice.org.uk/>

            select *
            from niceprov:
            from nice:
            where {
            ?entity prov:specializationOf @resource.
            ?entity ^prov:uses ?commit .
            ?commit a compilation:Commit .
            {
                select ?commit (count(?nextCommit) as ?count)
                where
                {
                    @head prov:informedBy* ?nextCommit .
                    ?nextCommit prov:informedBy* ?commit .
                }
                group by ?commit
            }
            }
            order by desc(?count)
            LIMIT 1
            """ [ ("resource", Param.Uri resource)
                  ("head", Param.Uri commit) ]
       |> ResultSet.singles
       |> asUri

    let clause =
      List.mapi (fun i v -> sprintf "optional { @entity %s ?o_%d . } " v i)
      >> String.concat "\n"

    let firstPathOPath (s:System.String) =
      PathRegex().Match(s).firstPartOfPropertyPath.Value

    let construct =
      List.mapi (fun i v -> sprintf " @entity <%s> ?o_%d . " (firstPathOPath v) i)
      >> String.concat "\n"


    let subGraph entity =
      let clause = clause propertyPaths
      let construct = construct propertyPaths
      let query = (sprintf """
                           prefix prov: <http://www.w3.org/ns/prov#>
                           prefix niceprov: <http://ld.nice.org.uk/prov>
                           prefix compilation: <http://ld.nice.org.uk/ns/compilation#>
                           prefix nice: <http://ld.nice.org.uk/>
                           prefix dcterms: <http://purl.org/dc/terms/>
                           construct {
                             @entity a ?t .
                             @entity prov:specializationOf ?r .
                             ?alt ^prov:alternateOf @entity .
                             %s
                           }
                           from <http://ld.nice.org.uk/ns>
                           from <http://ld.nice.org.uk/prov>
                           from <http://ld.nice.org.uk/>
                           where {
                             @entity a ?t .
                             @entity prov:specializationOf ?r .
                             optional { @entity prov:alternateOf ?alt .  } .
                             %s
                           }
                   """ construct clause) |> log.write Debug

      Graph.defaultPrefixes (Uri.from "http://ld.nice.org.uk/") [] (stardog.queryGraph [] query [ ("entity", Param.Uri entity) ])


    ///Append _id and _type, kill context for now as elastic doesn't like the remotes
    ///Would whine about type implicit from structure but this would be a bit hypocritical
    let elasiticerise (x : JObject) =
      x.["_id"] <- x.["prov:specializationOf"]
      x.Add("_type",JValue("qualitystatement"))
      x.Remove("@context") |> ignore
      x

    let opts = JsonLdOptions()
    opts.SetCompactArrays(true)
    opts.SetUseRdfType(false)
    opts.SetUseNativeTypes(true)
    opts.SetEmbed(System.Nullable<_>(true))
    opts.SetExplicit(System.Nullable<_>(false))
    let xr =
      (resources
       |> PSeq.collect ( entityForResource |> retry )
       |> PSeq.map ( subGraph |> retry )
       |> PSeq.map
            (Resource.fromType
               (Uri.from "http://www.w3.org/2002/07/owl#NamedIndividual"))
      |> Seq.filter (List.isEmpty >> not))
    PSeq.map
      ((Resource.compatctedJsonLD opts (Context(context, opts)))
       >> elasiticerise) xr
