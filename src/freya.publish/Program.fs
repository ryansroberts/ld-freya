module Freya.Main

open FSharp.RDF
open Nessos.UnionArgParser
open System.IO
open ExtCore
open Freya.Publication
open FSharp.Collections.ParallelSeq
open FSharp.Text.RegexProvider

type Arguments =
  | Connection of string
  | Db of string
  | User of string
  | Pass of string
  | Output of string
  | Commit of string
  | Context of string
  | PropertyPath of string
  | Debug
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Connection _ -> "Stardog server http endpoint"
      | Db _ -> "Stardog database"
      | User _ -> "Username"
      | Pass _ -> "Password"
      | Output _ -> "Perform the specified action"
      | Context _ -> "Remote json ld context to use"
      | Commit _ -> "Commit hash to publish"
      | PropertyPath _ -> "Property path to include in elastic record"
      | Debug _ -> "Print diagnostic information"

type iriP = Regex< "(?<prefix>.*):(?<fragment>.*)" >

open Newtonsoft.Json.Linq
open Newtonsoft.Json

[<EntryPoint>]
let main argv =
  System.Net.ServicePointManager.DefaultConnectionLimit <- System.Int32.MaxValue //Magic go faster switch, defaults to 3 http connections
  let parser = new UnionArgParser<Arguments>()
  let args = parser.Parse argv
  let logger = (if args.Contains <@ Debug @> then Logger.Debug else Info)
  let toLower (s : string) = s.ToLower()
  let containsParam param = Seq.map toLower >> Seq.exists ((=) (toLower param))
  let paramIsHelp param =
    containsParam param [ "help"; "?"; "/?"; "-h"; "--help"; "/h"; "/help" ]
  if ((argv.Length = 2 && paramIsHelp argv.[1]) || argv.Length = 1) then
    printfn """Usage: freya [options]
      %s""" (parser.Usage())
    exit 1
  let stardog =
    Store.Store.stardog (args.GetResult <@ Connection @>)
      (args.GetResult <@ Db @>) (args.GetResult <@ User @>)
      (args.GetResult <@ Pass @>)
  let commit =
    Uri.from
      (sprintf "http://ld.nice.org.uk/prov/commit#%s"
         (args.GetResult <@ Commit @>))
  let toPublish =
    publish logger stardog commit (args.GetResults <@ PropertyPath @>)
      (args.GetResults <@ Context @>)
  let output = args.GetResult <@ Output @>
  let iriP = iriP()
  toPublish |> Seq.iter (fun x ->
                 let id = (x.["_id"]).Value<string>() //ugh
                 printfn "Writing ld for %s" id
                 let m = iriP.Match id
                 use fout =
                   System.IO.File.CreateText
                     (System.IO.Path.Combine
                        (output, (sprintf "%s.jsonld" m.fragment.Value)))
                 use w = new JsonTextWriter(fout)
                 x.WriteTo w)
  0 // return an integer exit code
