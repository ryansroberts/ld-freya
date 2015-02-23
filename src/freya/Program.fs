open VDS.RDF
open common.RDF
open VDS.RDF.Writing
open VDS.RDF.Query
open Model

type common.RDF.Uri with
    static member fromVDS (n : INode) = 
        match n with
        | :? IUriNode as n -> Uri.Sys n.Uri
        | _ -> failwith "Not a uri node"

type Compilation
    with static member load (s : Store.store) = 
        let resultset = Store.resultset s
        printfn "%A" resultset
        let single (r : SparqlResult) = r.[0]
        let double (r : SparqlResult) = (r.[0], r.[1])
        { Id = 
            resultset "select ?id where {?id a compilation:Compilation}"
            |> Seq.exactlyOne
            |> single
            |> Uri.fromVDS
          Targets = 
            resultset """
                        select (?id,?chars)
                        where {
                        ?cmp a compilation:Compilation .
                        ?cmp prov:uses ?id .
                        ?id cnt:chars ?chars 
                        }
                        """
            |> Seq.map double
            |> Seq.map (function 
                    | (id, chars) -> 
                        { Id = Uri.fromVDS id
                          Path = Path((string) (Uri.fromVDS id))
                          Content = string chars })
            |> Seq.toList }

open Nessos.UnionArgParser

type Arguments = 
    | CompilationOntology of string
    | Provenance of string
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | CompilationOntology e -> "Path or url of compilation ontology"
            | Provenance p -> "Path or url to input provenance"

[<EntryPoint>]
let main argv =

    printfn "lol"
    let parser = UnionArgParser.Create<Arguments>()
    let args = parser.Parse argv

    let ont = (args.GetResult(<@ CompilationOntology @>))
    let prov = (args.GetResult(<@ Provenance @>))



    let prov = Store.loadFile prov
    Store.defaultNamespaces prov "http://nice.org.uk/ns/compilation#"
    Store.dump prov |> ignore

    prov 
    |> Compilation.load
    |> printfn "%A"
    0 // return an integer exit code
