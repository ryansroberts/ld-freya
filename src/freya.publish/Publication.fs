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
open JsonLD.Core
open Newtonsoft.Json.Linq

module Publication = 
  type PropertyPaths = 
    | PropertyPaths of Uri list list
  
  let publish (stardog : Store) commit propertyPaths contexts = 
    let contextS (uri) = sprintf """ "%s" """ uri
    let contexts = contexts |> List.map contextS
    let context = (JObject.Parse(sprintf """ {
                "@context": [
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

            select ?resource
            from <http://ld.nice.org.uk/ns>
            from <http://ld.nice.org.uk/prov>
            from <http://ld.nice.org.uk/>
            where {
            ?s prov:specializationOf ?resource .
            }
            """ []
       |> ResultSet.singles
       |> asUri)
    
    let entityForResource resource = 
      (stardog.queryResultSet [] """
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
       |> asUri)
    
    let clause = 
      List.mapi (fun i v -> sprintf "@entity %s ?o_%d ." v i) 
      >> List.fold (+) ""
    
    let subGraph entity = 
      let clause = clause propertyPaths
      Graph.defaultPrefixes (Uri.from "http://ld.nice.org.uk/") [] (stardog.queryGraph 
                                                                      [] (sprintf """
            prefix prov: <http://www.w3.org/ns/prov#>
            prefix niceprov: <http://ld.nice.org.uk/prov>
            prefix compilation: <http://ld.nice.org.uk/ns/compilation#>
            prefix nice: <http://ld.nice.org.uk/>
            prefix dcterms: <http://purl.org/dc/terms/>
            construct {
              @entity a ?t .
              @entity prov:specializationOf ?r .
              @entity prov:alternateOf ?alt .
              %s
            }
            from <http://ld.nice.org.uk/ns>
            from <http://ld.nice.org.uk/prov>
            from <http://ld.nice.org.uk/>
            where {
              @entity a ?t .
              @entity prov:specializationOf ?r .
              optional  { @entity prov:alternateOf ?alt .  }
              %s
            }
         """ clause clause) [ ("entity", Param.Uri entity) ])
    
    ///Append _id and _type
    let elasiticerise (x : JObject) = 
      x.["_id"] <- x.["prov:specializationOf"]
      x.["_type"] <- x.["@type"]
      x
    
    let opts = JsonLdOptions()
    opts.SetCompactArrays(true)
    opts.SetUseRdfType(false)
    opts.SetEmbed(System.Nullable<_>(true))
    let xr = 
      (resources
       |> Seq.collect entityForResource
       |> Seq.map subGraph
       |> Seq.map 
            (Resource.fromType 
               (Uri.from "http://www.w3.org/2002/07/owl#NamedIndividual")))
      |> Seq.filter (List.isEmpty >> not)
    Seq.map 
      ((Resource.compatctedJsonLD opts (Context(context, opts))) 
       >> elasiticerise) xr
