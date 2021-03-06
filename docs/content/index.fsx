(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Json"

(**
FSharp.Json
======================

Elm inspired Json encoder/decoder combinator library.

<!--
Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The FSharp.Json library can be <a href="https://nuget.org/packages/FSharp.Json">installed from NuGet</a>:
      <pre>PM> Install-Package FSharp.Json</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>
-->

Example
-------

This example demonstrates using a function defined in this sample library.

*)
#r "FSharp.Json.dll"
open FSharp.Json.Decode

let json = "[1,2,3,4,5,6,7,8,9,10]"
let parsed = decodeString (dlist dint) json
let result = returnOrFail parsed

printfn "answer = %A" result

(**
Some more info

Samples & documentation
-----------------------

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

<!-- * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions. -->

Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork
the project and submit pull requests. If you're adding a new public API, please also
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and
redistribution for both commercial and non-commercial purposes. For more information see the
[License file][license] in the GitHub repository.

  [content]: https://github.com/fsprojects/FSharp.Json/tree/master/docs/content
  [gh]: https://github.com/fsprojects/FSharp.Json
  [issues]: https://github.com/fsprojects/FSharp.Json/issues
  [readme]: https://github.com/fsprojects/FSharp.Json/blob/master/README.md
  [license]: https://github.com/fsprojects/FSharp.Json/blob/master/LICENSE.txt
*)
