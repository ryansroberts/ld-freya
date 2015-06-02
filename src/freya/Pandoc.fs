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

      match args.StdIn with
      | Some x ->
        try
           x.CopyTo proc.StandardInput.BaseStream
           proc.StandardInput.Close ()
        with _ ->  ()
      | None -> ()

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
      Commit : string
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

  let private parser = UnionArgParser.Create<PandocArgs>()

  open Assertion
  open Assertion.rdf
  open FSharp.RDF.resource

  let convertResources r xr = function
    | (p, conv) ->
      let fragment (Uri.Sys u) = u.Fragment.Substring(1, u.Fragment.Length - 1)

      let file root =
        let fn = Freya.FullName(Resource.id r |> fragment, extension p)
        root ++ (Path.from (mimeTypeDir p)) ++ (Path.from conv.Commit)  ++ fn


      let generatedBy =
        match p with
        | Pdf -> !"compilation:Pdf"
        | HtmlDocument -> !"compilation:HtmlDocument"
        | Docx -> !"compilation:Docx"
        | HtmlFragment -> !"compilation:HtmlFragment"

      let generationProv =
        blank !"prov:hadGeneration"
          [ a !"prov:Generation"
            a !"prov:InstantaneousEvent"
            dataProperty !"prov:atTime" (DateTimeOffset.Now ^^ xsd.datetime)
            objectProperty !"prov:activity" generatedBy
            objectProperty !"prov:specialisationOf"
              !("http://ld.nice.org.uk/" + string (file (Freya.Path []))) ]

      let args =
        [ From "markdown"
          Output(string (Path.ensurePathExists (file conv.Output)))
          Smart
          Normalize
          Self_Contained ]
        @ match p with
          | Pdf -> [ Latex_Engine "xelatex" ]
          | HtmlDocument ->
            [ Standalone
              To "html5" ]
          | Docx -> [ To "docx" ]
          | HtmlFragment -> [ To "html5" ]

      async {
        if File.exists (file conv.Output) then
        return [info (sprintf "Artifact already exists, skipping - %s" (file conv.Output |> string)) (resourceLocation r)]

        else match r with
        | FunctionalDataProperty (Uri.from "cnt:chars") (FSharp.RDF.xsd.string)
          content ->
          let! (exit,stdout,stderr) = asyncShellExec { Program = "pandoc"
                                                       WorkingDirectory = string (conv.WorkingDir)
                                                       CommandLine =
                                                         parser.PrintCommandLine args |> String.concat " "
                                                       StdIn =
                                                         new MemoryStream(System.Text.Encoding.UTF8.GetBytes
                                                         content) :> Stream |> Some }
          let log =
            match exit with
            | 0 -> info
            | _ -> error
          return [ log
                     (sprintf "Pandoc conversion \r %s \r %s"
                        (String.concat "" stdout) (String.concat "" stderr))
                     (resourceLocation r)
                   generationProv ]
        | _ ->
          return [ error
                     (sprintf "No content statement - failed to convert %A" r)
                     (resourceLocation r) ]
      }
