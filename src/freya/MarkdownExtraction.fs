namespace Freya

open FSharp.Data
open Newtonsoft.Json.Linq
open FSharp.RDF
open FSharp.RDF.Assertion
open Freya
open FSharp.Markdown
open FSharp.RDF.Assertion
open System.Text.RegularExpressions

module Markdown =
  let level f =
    function
    | Some(i, x) when f i -> Some x
    | _ -> None

  let rec textN =
    function
    | MarkdownSpan.Literal x -> x
    | MarkdownSpan.Strong xs -> text xs
    | MarkdownSpan.Emphasis xs -> text xs
    | MarkdownSpan.EmbedSpans xs -> text (xs.Render())
    | MarkdownSpan.HardLineBreak -> "\n"
    | _ -> ""

  and text = List.map textN >> String.concat ""

  let rec pTextN =
    function
    | MarkdownParagraph.Paragraph xs -> text xs
    | MarkdownParagraph.CodeBlock(x, _, _)
    | MarkdownParagraph.InlineBlock(x)
    | MarkdownParagraph.InlineBlock x -> x
    | MarkdownParagraph.Heading(_, xs) | MarkdownParagraph.Paragraph xs | MarkdownParagraph.Span xs ->
      text xs
    | MarkdownParagraph.QuotedBlock xs -> pText xs
    | MarkdownParagraph.ListBlock(_, xs) -> pText' xs
    | MarkdownParagraph.TableBlock(x, _, xs) -> ""
    | MarkdownParagraph.EmbedParagraphs(xs) -> ""
    | _ -> ""

  and pText = List.map pTextN >> String.concat ""

  and pText' = List.map pText >> String.concat ""

  type MarkdownSpan with
    static member text = text
    static member re s xs =
      if ((Regex.IsMatch(MarkdownSpan.text xs, s))) then Some xs
      else None

  type SpanSelector = MarkdownParagraph -> MarkdownSpans option

  type SpanFilter = MarkdownSpans option -> MarkdownSpans option

  type MarkdownParagraph with

    static member heading =
      (function
      | MarkdownParagraph.Heading(i, x) -> Some(i, x)
      | _ -> None)

    static member hAny = MarkdownParagraph.heading >> level (fun f -> true)
    static member h1 = MarkdownParagraph.heading >> level ((=) 1)
    static member h2 = MarkdownParagraph.heading >> level ((=) 2)
    static member h3 = MarkdownParagraph.heading >> level ((=) 3)
    static member text = pTextN
    static member following f =
      function
      | x :: xs ->
        match f (x : MarkdownParagraph) with
        | Some _ -> xs
        | None -> MarkdownParagraph.following f xs
      | _ -> []

  let inline (>>=) f g x =
    match f x with
    | Some x -> g x
    | _ -> None
