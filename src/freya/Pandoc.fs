namespace Freya

open System
open System.ComponentModel
open System.Diagnostics
open System.IO
open System.Threading
open System.Collections.Generic
open Nessos.UnionArgParser
open ExtCore
open System.Threading.Tasks
open FSharp.RDF
open Freya.Tracing
open Freya

module Pandoc =
  let inline awaitPlainTask (task : Task) =
    // rethrow exception from preceding task if it fauled
    let continuation (t : Task) : unit =
      match t.IsFaulted with
      | true -> raise t.Exception
      | arg -> ()
    task.ContinueWith continuation |> Async.AwaitTask

  /// Parameter type for process execution.
  type ExecParams =
    { /// The path to the executable, without arguments.
      Program : string
      /// The working directory for the program. Defaults to "".
      WorkingDirectory : string
      /// Command-line parameters in a string.
      CommandLine : string
      StdIn : string }

  type ProcessResult = int * string list * string list

  let startedProcesses = HashSet()

  let start (proc : Process) =
    proc.Start() |> ignore
    startedProcesses.Add(proc.Id, (proc.StartTime)) |> ignore

  let asyncShellExec (args : ExecParams) =
    async {
      let stdout = new System.Text.StringBuilder()
      let stderr = new System.Text.StringBuilder()
      if String.IsNullOrEmpty args.Program then
        invalidArg "args" "You must specify a program to run!"
      let info =
        ProcessStartInfo
          (args.Program, UseShellExecute = false, RedirectStandardError = true,
           RedirectStandardOutput = true,
           WindowStyle = ProcessWindowStyle.Hidden,
           WorkingDirectory = args.WorkingDirectory,
           Arguments = args.CommandLine, RedirectStandardInput = true)
      use proc = new Process(StartInfo = info)
      proc.ErrorDataReceived.Add(fun e ->
        if e.Data <> null then stderr.Append e.Data |> ignore)
      proc.OutputDataReceived.Add(fun e ->
        if e.Data <> null then stdout.Append e.Data |> ignore)
      start proc

      proc.BeginOutputReadLine()
      proc.BeginErrorReadLine()
      do! proc.StandardInput.BaseStream.AsyncWrite(System.Text.Encoding.UTF8.GetBytes args.StdIn)
      proc.StandardInput.Close()
      // attaches handler to Exited event, enables raising events, then awaits event
      // the event gets triggered even if process has already finished
      let! _ = Freya.Async.GuardedAwaitObservable proc.Exited
                 (fun _ -> proc.EnableRaisingEvents <- true)
      return (proc.ExitCode, stdout.ToString(), stderr.ToString())
    }

  type PandocArgs =
    | From of string
    | To of string
    | Output of string
    | Data_Dir of string
    | Working_Directory of string
    | Smart
    | Latex_Engine of string
    | Filter of string
    | Normalize
    | Extract_Media
    | Standalone
    | Template of string
    | Toc
    | Toc_Depth of int
    | Include_In_Header of string
    | Self_Contained
    | Reference_Links
    | Number_Sections
    | Id_Prefix
    | Css of string
    | Reference_Docx of string
    | Bibliography of string
    interface IArgParserTemplate with
      member s.Usage = "" //Only using for IPC

  type ConversionArgs =
    { Output : Freya.Path
      ToolMatch : ToolMatch
      WorkingDir : Freya.Path }

  [<AutoOpen>]
  module pandocconversion =
    let mimeTypeDir =
      function
      | Docx ->
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
      | Pdf -> "application/pdf"
      | HtmlFragment | HtmlDocument -> "text/html"

    let extension =
      function
      | Docx -> ".docx"
      | Pdf -> ".pdf"
      | HtmlDocument | HtmlFragment -> ".html"

  let private parser = new UnionArgParser<PandocArgs>()

  open Assertion
  open Assertion.rdf
  open FSharp.RDF.resource

  let convertResources r xr =
    function
    | (t, conv) ->
      let compilationMessages (x : Statement list) = (x, [])
      let target = conv.ToolMatch.Target
      let file root =
        root ++ Path.from (Uri.fragment conv.ToolMatch.Target.Commit)
        ++ Path.from (mimeTypeDir t) ++ File.path target.Path
        ++ FullName(File.name target.Path, extension t)

      let args =
        [ From "markdown"
          Output(string (Path.ensurePathExists (file conv.Output)))
          Smart
          Normalize ]
        @ match t with
          | Pdf -> [ Latex_Engine "lualatex" ]
          | HtmlDocument ->
            [ Standalone
              Self_Contained
              To "html5" ]
          | Docx -> [ To "docx" ]
          | HtmlFragment -> [ To "html5" ]

      let resourceUri =
        Uri.from
          ("http://ld.nice.org.uk" + (string (file (Path.from "/artifacts"))))
      let generationProv = generatedResource conv.ToolMatch resourceUri t
      if File.exists (file conv.Output) then
        (generationProv [ warn "Resource already exists" (resourceLocation r) ])
      else
        let (exit, stdout, stderr) =
          asyncShellExec { Program = "pandoc"
                           WorkingDirectory = string (conv.WorkingDir)
                           CommandLine =
                             parser.PrintCommandLine args |> String.concat " "
                           StdIn = (snd conv.ToolMatch.Target.Content) }
          |> Async.RunSynchronously

        let log =
          match exit with
          | 0 -> info
          | _ -> error

        (generationProv
           [ log (sprintf "Pandoc conversion %A \r %s \r %s" args stdout stderr)
               (resourceLocation r) ])
