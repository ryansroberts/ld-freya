namespace System

open System.Reflection

[<assembly:AssemblyTitleAttribute("freya.publish")>]
[<assembly:AssemblyProductAttribute("freya")>]
[<assembly:AssemblyDescriptionAttribute("Make tool for linked data")>]
[<assembly:AssemblyVersionAttribute("1.0")>]
[<assembly:AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation = 
  [<Literal>]
  let Version = "1.0"
