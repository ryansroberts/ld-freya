#I "../../../bin/"
#I "../../../packages/FSharp.RDF/lib/"
#r "../../../packages/FSharp.RDF/lib/FSharp.RDF.dll"
#r "../../../bin/freya.exe"

open Freya.Builder
open Freya.YamlParser
open FSharp.RDF
open FSharp.RDF.Assertion

extractor "CustomExtractor" (fun x -> {
  Trace = []
  Extracted = [owl.individual !!"http://test.result/custom" [] [] ]
})

yamlExtractor "CustomYaml" (fun x -> {
  Trace  = []
  Extracted = [owl.individual !!"http://test.result/yaml" [] [] ]
})

target "QualityStandards" (dir "qualitystandards")
target "QualityStatement" (file "statement_$(QualityStatementId).md"
                                ["CustomExtractor";"CustomYaml"]
                                (Some "file://templates/qs.md")
                                "http://ld.nice.org.uk/ns/qualitystandards/QualityStatement")
target "QualityStandard"  (dir "standard_$(QualityStandardId)")

"QualityStandards"
===> ["QualityStandard"
      ===> ["QualityStatement"]]

