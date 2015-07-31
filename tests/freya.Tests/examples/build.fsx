#I "../../../bin/"
#I "../../../packages/FSharp.RDF/lib/net40/"
#r "../../../bin/freya.exe"
open Freya.Builder

target "QualityStandards" (dir "qualitystandards")
target "QualityStatement" (file "statement_$(QualityStandardId).md"
                                [content;yamlMetadata;]
                                (Some "file://templates/qs.md")
                                "http://ld.nice.org.uk/ns/qualitystandards/QualityStatement")
target "QualityStandard"  (dir "standard_$(QualityStandardId)")

"QualityStandards"
===> ["QualityStandard"
      ===> ["QualityStatement"]]

