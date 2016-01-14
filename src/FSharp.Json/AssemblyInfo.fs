namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.Json")>]
[<assembly: AssemblyProductAttribute("FSharp.Json")>]
[<assembly: AssemblyDescriptionAttribute("Elm like Json encoder/decoder combinator library")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
