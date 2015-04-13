namespace Freya

open ExtCore
open FSharp.Markdown
open System.Text



(*
    #Some heading

    Paragraph Text 1

    Paragraph Text 2

    "#[Some heading]" = Heading[Span "Some"
        Span "heading"] * )

module MarkdownExtraction =
    let spansAsText = List.map string >> List.fold(+)
""

type SpanSelector = | Regex of(Span list - > bool)
with static member from s = ()


type ParagraphSelector = | Heading of SpanSelector
*)
