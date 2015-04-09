
open VDS.RDF
open VDS.RDF.Writing
open VDS.RDF.Query
open FSharp.RDF
open Nessos.UnionArgParser
open Assertion
open Freya
open Freya.compilation
open Freya.tools

type Delta =
    { From : Uri
      To : Uri
      Path : Path }

type OntologyVersion =
    { Version : Uri
      Path : Path }

type CompilationOutput =
    { Path : Path
      Tip : OntologyVersion
      WorkingArea : Delta }

let fragment (Uri.Sys u) = u.Fragment
let removeHash (s:string) =  s.Substring(1,(s.Length - 1))

let deltafile prov =
    let c = fragment (prov.Commits.Head.Id) |> removeHash
    let c' = fragment (prov.Commits |> Seq.last).Id |> removeHash
    sprintf "%s-%s" c c'

let saveCompilation p prov pr r : unit =
    let kbg = graph.empty (!"http://nice.org.uk/") []
    for xe,xo in r do
      Assert.resources kbg xo |> ignore
      Assert.resources pr xe |> ignore
    let d = deltafile prov
    let kbn = (sprintf "%s/%s.ttl" (string p) d)
    let prn = (sprintf "%s/%s.prov.ttl" (string p) d)
    printfn "Writing compilation to %s and prov to %s" kbn prn
    graph.format graph.write.ttl (graph.toFile kbn) kbg |> ignore
    graph.format graph.write.ttl (graph.toFile prn) pr |> ignore


let toFile (p) = new System.IO.StreamWriter(System.IO.File.OpenWrite p)
let (++) a b = System.IO.Path.Combine(a, b)

let compile pth m p d =
    let pr = (loadProvenance p)
    let xr = makeAll m pr.Targets
    let byFailure = function | Failure(_) -> true | _ -> false
    let result = List.map ( function | Success s -> s | Failure f -> f  )

    printfn "Tool configuration %A" m

    pr.Targets |> List.iter (fun t -> printfn "Compiling: %s" (string t.Path))

    let (xf,xs) = List.partition byFailure xr

    match (result xf),(result xs)  with
    | x :: xs, _ ->
      [ for (xe,_) in x :: xs do yield! xe ]
      |> Assert.resources p
      |> graph.format graph.write.ttl System.Console.Error
      |> ignore
      1
    | [], x :: xs ->
      [ for s in x :: xs -> s ]
      |> saveCompilation pth pr p
      |> ignore
      0
    | _ -> 0

type Arguments =
    | Compilation of string
    | Provenence of string
    | Describe of string
    | Action of string
    | Output of string
    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Compilation e -> "Path or url of compilation ontology"
        | Provenence t -> "Path or url to input provenance"
        | Describe t -> "Display actions available at path"
        | Action t -> "Perform the specified action"
        | Output t -> "Directory to save compilaton output"

[<EntryPoint>]
let main argv =
    let parser = UnionArgParser.Create<Arguments>()
    let args = parser.Parse argv
    let makeOntology =
      graph.loadFrom (args.GetResult(<@ Compilation @>)) |> loadMake
    let toLower (s : string) = s.ToLower()
    let containsParam param =
      Seq.map toLower >> Seq.exists ((=) (toLower param))
    let paramIsHelp param =
      containsParam param [ "help"; "?"; "/?"; "-h"; "--help"; "/h"; "/help" ]
    if ((argv.Length = 2 && paramIsHelp argv.[1]) || argv.Length = 1) then
      printfn """Usage: freya [options]
                %s""" (parser.Usage())
      exit 1
    compile (args.GetResult <@ Output @> |> Path.from) makeOntology
      (args.GetResult <@ Provenence @> |> graph.loadFrom)
      (args.GetResult <@ Output @>)
