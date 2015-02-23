module Rules
open Model
open Tools

type GraphUpdate = VDS.RDF.Graph -> unit

let alwaysEchoContent (c : Target) = Model.EchoContent
let execute = function 
| c, EchoContent -> Tools.echoContent c

