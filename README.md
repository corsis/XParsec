```
Version 2 of this library is being built as part of a larger fundraising project.
```

- [x] Rename primitives for clarity
- [ ] Comprehensive documentation
- [x] Generalized parser negation `-`
- [x] Steps in `uint64` instead of `int` (to be used with very large data sets)
- [x] XParsec.fs (primitives; operators; combinators)
- [x] XParsec.Array.fs [parse any 1D source]
- [x] XParsec.Xml.Linq.fs [parse System.Xml.Linq trees]
- [ ] [XParsec.Fable.Html.fs](https://home.corsis.tech/fable.html) (click last link) [parse browser DOM trees]
- [ ] [XParsec + PDF.js (tech demo)](https://home.corsis.tech/) [parse PDF pages, PDF documents]
      ![docpar]

---

```
Version 1 examples below.
```

**XParsec** works with **[any type](https://github.com/corsis/XParsec/blob/master/XParsec.fsi#L26)**, is **[very easy to extend](https://github.com/corsis/XParsec/blob/master/XParsec.fs#L81)**, supports **[domain-specific non-linear navigation](https://github.com/corsis/XParsec/blob/master/XParsec.fs#L122)** and is **[implemented in a single F# file with just ~100 source lines of code](https://github.com/corsis/XParsec/blob/master/XParsec.fs)**.

(FParsec only works with `Char`s and can only go forward on a one dimensional `String`.)

# Example 1

**[XParsec.Xml](https://github.com/corsis/XParsec/blob/master/XParsec.fsi#L65)** is the first XParsec extension. It is **implemented in just 14 source lines of code** for the examples used below and provides complete freedom in navigating XML trees.

```fsharp
open XParsec
open XParsec.Xml

[<EntryPoint>]
let main _ =

  let test parse = printfn "%A" << reply << parse << E.source

  let root = E.Parse "<root><a><b><c><d font='Arial'></d></c></b></a></root>"

  //            domain-specific
  //              navigation
  //                  v
  let parser1 = many (child => name) .>. !@"font"
  //            ^           ^
  //         powerful     first-class
  //      combinators     extensibility

  // graceful choices
  let parser2 = (parent => name) </> (!*child >. !@"font")

  // graceful non-linear look-ahead (here = down in Xml)
  let parser3 = !!parser1 .>. (current => name)

  // brand-new non-linear look-back (here = up   in Xml)
  let S d,_   = E.source root |> (!*child >. current)
  let parser4 = !!(many (parent => name)) .>. (current => name)

  test parser1 root; test parser2 root; test parser3 root; test parser4 d; 0
```
```fsharp
S (["a"; "b"; "c"; "d"], "Arial")
S "Arial"
S ((["a"; "b"; "c"; "d"], "Arial"), "root")
S (["c"; "b"; "a"; "root"], "d")
```

# Example 2

Recursion &ndash; handled with ease.

```fsharp
open XParsec
open XParsec.Xml

type Xobj  = I of int | L of Xobj list

[<EntryPoint>]
let main _ =

  let root = E.Parse "<list><int v='1'/><list><int v='2'/></list><int v='3'/></list>"

  let e,e' = future ()

  let int_ = !<>"int"  >. !@"v"      => (Int32.Parse >> I)
  let list = !<>"list" >. children e =>                 L

  do  e'  := int_ </> list

  test e root; 0
```
```fsharp
S (L [I 1; L [I 2]; I 3])
```

# Browse

+ [Signatures](https://github.com/corsis/XParsec/blob/master/XParsec.fsi#slider)
+ [Implementation](https://github.com/corsis/XParsec/blob/master/XParsec.fs#slider)

# License

```
XParsec™ © 2012 – 2018 Cetin Sert

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * The names of contributors may not be used to endorse or promote
      products derived from this software without specific prior
      written permission. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

# Contact

[![corsis]](https://github.com/corsis)

[![cssign]](https://github.com/cetinsert)

[xparsec@corsis.tech](mailto:xparsec@corsis.tech)

[corsis]: https://home.corsis.tech/marketing/images/ipw/karamalz/corsis.png "Corsis Research"
[cssign]: https://home.corsis.tech/marketing/images/ipw/karamalz/sig.100.tr.png "Cetin Sert"
[docpar]: https://home.corsis.tech/marketing/images/ipw/hansol/cofs1.png "Document Parsing"
