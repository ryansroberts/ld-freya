#r "../../packages/dotNetRDF/lib/net40/dotNetRDF.dll"
#r "../../packages/VDS.Common/lib/net40-client/VDS.Common.dll"
#r "../../packages/FSharp.RDF/lib/net40/FSharp.RDF.dll"
#r "../../packages/Unquote/lib/net40/Unquote.dll"
#r "../../packages/FSharp.Formatting/lib/net40/FSharp.Markdown.dll"
#r "../../packages/SharpYaml/lib/SharpYaml.dll"
#r "../../packages/ExtCore/lib/net40/ExtCore.dll"
#r "../../packages/UnionArgParser/lib/net40/UnionArgParser.dll"
#I "../../packages/FParsec/lib/net40-client/"
#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/FParsec/lib/net40-client/FParsecCS.dll"

open FSharp.RDF
open FParsec

type Expression =
  | Seq of Expression list
  | Wildcard of string
  | Literal of string
  | Variable of string

let str = pstring
let pLiteral<'a> = many1Chars letter |>> Literal
let pVariable<'a> = between (str "$(") (str ")") ((many1Chars letter ) |>> Variable)
let pWildcard<'a> = str "*" |>> Wildcard
let pExpressionC = choice [pWildcard;pVariable;pLiteral]
let pSeq = many pExpressionC |>> Seq
let pExpression = pSeq .>> eof

let test str p =
  match run p str with
  | Success(result, _, _)   -> printfn "Success: %A" result
  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test "abigfatliteral" pExpression
test "*" pExpression
test "*$(bob)" pExpression
test "*rest" pExpression
test "$(bob)" pExpression
test "SomePath$(bob)RestOfPath" pExpression
