namespace Freya

open ExtCore
open FSharp.Markdown
open System.Text



(*
    #Some heading

    Paragraph Text 1

    Paragraph Text 2

    "#[Some heading]" = Heading[Span "Some"
        Span "heading"] *)

(*
module MarkdownExtraction =

  type Axis =
    | Block

  type SpanSelector =
    | Regex of System.Text.RegularExpressions.Regex

  type Selector =
    | Heading of SpanSelector

  type Expression =
    | Selection of Selector * Axis
    | Sequence of Expression * Expression

  type blockSelection = Paragraph list -> Paragraph list

  let evaluateSS = function
    | Regex r -> (fun xs ->
                  let spansAsText = List.map string >> List.fold (+) ""
                  if (r.IsMatch (spansAsText xs)) then xs else []
                 )

  let evaluateA = function
    | Block -> (fun xs -> xs)

  let evaluateS = function
    | Heading ss -> (fun xs ->
                     match evaluateSS ss xs with
                     | x::xs -> xs | _ -> []
                     )

  let evaluateE = function
    | Selection s,a -> evaluateS s >> evaluateA a
    | Sequence a,b -> evaluateS a >> evaluateS b
*)
