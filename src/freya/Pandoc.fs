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
open resource
open Freya.Tracing

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
      ///Optional stream to pipe to stdin
      StdIn : Stream option }

  type ProcessResult = int * string list * string list

  let startedProcesses = HashSet()

  /// [omit]
  let start (proc : Process) =
    if proc.StartInfo.FileName.ToLowerInvariant().EndsWith(".exe") then
      proc.StartInfo.Arguments <- "--debug \"" + proc.StartInfo.FileName + "\" "
                                  + proc.StartInfo.Arguments
      proc.StartInfo.FileName <- "mono"
    proc.Start() |> ignore
    startedProcesses.Add(proc.Id, (proc.StartTime)) |> ignore

  let asyncShellExec (args : ExecParams) =
    async {
      let stdout = ref []
      let stderr = ref []
      if String.IsNullOrEmpty args.Program then
        invalidArg "args" "You must specify a program to run!"
      let info =
        ProcessStartInfo
          (args.Program, UseShellExecute = false, RedirectStandardError = true,
           RedirectStandardOutput = true,
           WindowStyle = ProcessWindowStyle.Hidden,
           WorkingDirectory = args.WorkingDirectory,
           Arguments = args.CommandLine,
           RedirectStandardInput = (Option.isSome args.StdIn))
      use proc = new Process(StartInfo = info)
      proc.ErrorDataReceived.Add(fun e ->
        if e.Data <> null then stderr := e.Data :: !stderr)
      proc.OutputDataReceived.Add(fun e ->
        if e.Data <> null then stdout := e.Data :: !stdout)
      start proc
      proc.BeginOutputReadLine()
      proc.BeginErrorReadLine()
      let! copy = async {
                    match args.StdIn with
                    | Some x ->
                      try
                        do! x.CopyToAsync proc.StandardInput.BaseStream
                            |> awaitPlainTask
                        do proc.StandardInput.Close()
                      with _ -> () //Process may have exited before we pipe to it
                    | _ -> ()
                  }
      // attaches handler to Exited event, enables raising events, then awaits event
      // the event gets triggered even if process has already finished
      let! _ = Freya.Async.GuardedAwaitObservable proc.Exited
                 (fun _ -> proc.EnableRaisingEvents <- true)
      return (proc.ExitCode, !stdout, !stderr)
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
      | Docx -> "docx"
      | Pdf -> "pdf"
      | HtmlDocument | HtmlFragment -> "html"

  let convertResources r xr = function
    | (p, conv) ->
      let fragment (Uri.Sys u) = u.Fragment.Substring(1, u.Fragment.Length - 1)

      let file =
        let fn = Freya.FullName(resourceId r |> fragment, extension p)
        let p = conv.Output ++ (Freya.path.toPath (mimeTypeDir p))
        p ++ fn

      let parser = UnionArgParser.Create<PandocArgs>()

      let args =
        [ From "markdown"
          Output(string (ensurePathExists file))
          Smart
          Normalize
          Self_Contained ]
        @ match p with
          | Pdf -> [ Latex_Engine "pdflatex" ]
          | HtmlDocument ->
            [ Standalone
              To "html5" ]
          | Docx -> [ To "docx" ]
          | HtmlFragment -> [ To "html5" ]
      async {
        match r with
        | FunctionalDataProperty (Uri.from "cnt:chars") (xsd.string) content ->
          let! (exit, stdout, stderr) = asyncShellExec
                                          { Program = "pandoc"
                                            WorkingDirectory =
                                              string (conv.WorkingDir)
                                            CommandLine =
                                              parser.PrintCommandLine args
                                              |> String.concat " "
                                            StdIn =
                                              new MemoryStream(System.Text.Encoding.UTF8.GetBytes
                                                                 content) :> Stream
                                              |> Some }
          let prov =
            match exit with
            | 0 -> info
            | _ -> error
          return prov
                   (sprintf "Pandoc conversion: \r %s \r %s"
                      (String.concat "" stdout) (String.concat "" stderr))
                   (resourceLocation r)
      }
