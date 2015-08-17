namespace Freya

open ExtCore
open FSharp.Markdown
open System.Text

module MarkdownExtraction =
  type Axis =
    | Block

  type SpanSelector =
    | Regex of System.Text.RegularExpressions.Regex
    override x.ToString() =
      match x with
      | Regex x -> (string x)

  type Selector =
    | Heading of SpanSelector
    override x.ToString() =
      match x with
      | Heading x -> sprintf "#[%s]" (string x)

  type Expression =
    | Selection of Selector * Axis
    | Sequence of Expression * Expression
    override x.ToString() =
      match x with
      | Selection(a, Block) -> sprintf "%s" (string a)
      | Sequence(a, b) -> sprintf "%s/%s" (string a) (string b)

  type blockSelection = MarkdownParagraph list -> MarkdownParagraph list

  let evaluateSS = function
    | Regex r ->
      (fun xs ->
      let spansAsText = List.map string >> List.fold (+) ""
      if (r.IsMatch(spansAsText xs)) then xs
      else [])
  let evaluateA = function
    | Block -> (fun xs -> xs)
  let evaluateS = function
    | Heading ss ->
      (fun xs ->
      match evaluateSS ss xs with
      | x :: xs -> xs
      | _ -> [])

  let rec evaluateE =
    function
    | Selection(s, a) -> evaluateS s >> evaluateA a
    | Sequence(a, b) -> evaluateE a >> evaluateE b
