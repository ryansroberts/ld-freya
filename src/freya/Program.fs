module Tools = 
  open Model
  open RDF
  
  let echoContent (c : Target) g = 
    let triples = triples g
    let puri = puri g
    let qn = qn g
    let literal = literal g
    let a = a g
    triples (puri c.Id, 
             [ (a, qn "cnt:ContentAsText")
               (qn "cnt:chars", literal c.Content) ])

module Rules = 
  open Model
  open Tools
  
  type GraphUpdate = VDS.RDF.Graph -> unit
  
  let alwaysEchoContent (c : Target) = Model.EchoContent
  let execute = function 
    | c, EchoContent -> Tools.echoContent c

module Store = 
  open VDS.RDF
  open VDS.RDF.Query
  open VDS.RDF.Query.Datasets
  open VDS.RDF.Storage
  open VDS.RDF.Storage.Management
  open VDS.RDF.Parsing
  
  let parser = SparqlQueryParser()
  
  type store = 
    | Memory of IGraph
    member x.QueryProcessor() = 
      match x with
      | Memory m -> LeviathanQueryProcessor(InMemoryDataset(m))
  
  let defaultUri = null :> System.Uri
  let loadGraph (g : Graph) = store.Memory g
  
  let loadFile (s : string) = 
    let g = new Graph()
    match s.StartsWith("http") with
    | true -> g.LoadFromUri(System.Uri s)
    | false -> g.LoadFromFile s
    Memory g
  
  let query (store : store) (q : string) = 
    (store.QueryProcessor()).ProcessQuery(parser.ParseFromString(q))
  let construct (store : store) q = query store q :?> IGraph
  let resultset (store : store) q = query store q :?> SparqlResultSet
  
  let dump g = 
    let s = System.Text.StringBuilder()
    let w = new VDS.RDF.Writing.CompressingTurtleWriter()
    use sw = new System.IO.StringWriter(s)
    w.Save(g, sw)
    s.ToString()

open VDS.RDF.Parsing
open VDS.RDF
open VDS.RDF.Query
open Model

let loadGraph (st : System.IO.Stream) = 
  let p = TurtleParser()
  use sr = new System.IO.StreamReader(st)
  let g = Graph()
  p.Load(g, sr)
  g

type Model.Uri with
  static member fromVDS (n : INode) = 
    match n with
    | :? IUriNode as n -> Model.Uri(string n)
    | _ -> failwith "Not a uri node"

let loadCompilation (g : Graph) = 
  let g = Store.loadGraph g
  let resultset = Store.resultset g
  let single (r : SparqlResult) = r.[0]
  let double (r : SparqlResult) = (r.[0], r.[1])
  { Id = 
      resultset "select ?id where {?id a compilation:Compilation}"
      |> Seq.exactlyOne
      |> single :?> IUriNode
      |> Uri.fromVDS
    Targets = 
      resultset """
                    select (?id,?chars)
                    where {
                       ?cmp a compilation:Compilation .
                       ?cmp prov:uses ?id .
                       ?id cnt:chars ?chars .
                    }
                    """
      |> Seq.map double
      |> Seq.map (function 
           | (id, chars) -> 
             { Id = Uri.fromVDS id
               Path = Path((string) (Uri.fromVDS id))
               Content = string chars })
      |> Seq.toList }

[<EntryPoint>]
let main argv = 
  printfn "%A" argv
  0 // return an integer exit code
