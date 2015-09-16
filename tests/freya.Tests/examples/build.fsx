#I "../../../bin/"
#I "../../../packages/FSharp.RDF/lib/"
#r "../../../packages/FSharp.RDF/lib/FSharp.RDF.dll"
#r "../../../bin/FSharp.Markdown.dll"
#r "../../../bin/ExtCore.dll"
#r "../../../bin/freya.exe"

open Freya.Builder
open Freya.YamlParser
open FSharp.RDF
open FSharp.RDF.Assertion
open FSharp.Markdown
open Freya.Markdown
open ExtCore

extractor "CustomExtractor" (fun x -> {
  Trace = []
  Extracted = [owl.individual !!"http://test.result/custom" [] [] ]
})

yamlExtractor "CustomYaml" (fun x -> {
  Trace  = []
  Extracted = [owl.individual !!"http://test.result/yaml" [] [] ]
})

markdownExtractor "CustomMD" (fun x -> 
  let p1 = x.Content.Paragraphs
           |> MarkdownParagraph.following
                   (MarkdownParagraph.hAny >>= (MarkdownSpan.re ".*(Q|q)uality.*(S|s)tatement.*"))
           |> List.map MarkdownParagraph.text
           |> List.map (fun x -> rdf.dataProperty !!"http://purl.org/dc/terms/abstract" (x^^xsd.string) )
           |> List.tryHead
  {
  Trace  = []
  Extracted = [owl.individual !!"http://test.result/markdown" [] [] ]})

target "QualityStandards" (dir "qualitystandards")
target "QualityStatement" (file "statement_$(QualityStatementId).md"
                                ["CustomExtractor";"CustomYaml";"CustomMD"]
                                (Some "file://templates/qs.md")
                                "http://ld.nice.org.uk/ns/qualitystandards/QualityStatement")
target "QualityStandard"  (dir "standard_$(QualityStandardId)")

"QualityStandards"
===> ["QualityStandard"
      ===> ["QualityStatement"]]

