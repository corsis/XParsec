**XParsec** works with [**any type**](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fsi#L26), is [**very easy to extend**](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fs#L102) and supports **[domain-specific non-linear navigation](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fsi#L88)**.

(FParsec only works with `Char`s and can only go forward on a one dimensional string.)

# Example

Here we use [`XParsec.Xml`](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fsi#L61) which provides the first XParsec extension [implemented in just 19 lines of F#](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fs#L102).

```fsharp
  open XParsec
  open XParsec.Xml

  let show reply = printfn "%A" reply
  let test parse = show << reply << parse << enter
  let name (e:E) = string e.Name                              // E = XElement

  let root = E.Parse "<root><a><b><c><d font='Arial'></d></c></b></a></root>"

  //             domain-specific
  //                navigation
  //                    v
  let parser1 = many (child => name) .>. !@"font"
  //             ^              ^
  //         powerful        first-class
  //        combinators     extensibility

  // graceful choices
  let parser2 = !*child >. !@"font"
  let parser3 =  parent => name </> parser2

  // graceful non-linear look-ahead (here = down in Xml)
  let parser4 = attempt parser1 .>. (current => name)

  // brand-new non-linear look-back (here = up   in Xml)
  let S d,_   = enter root |> (!*child >. current)
  let parser5 = attempt <| many (parent => name) .>. (current => name)

  test parser1 root; test parser2 root; test parser3 root
  test parser4 root; test parser5 d
```
```fsharp
S (["a"; "b"; "c"; "d"], "Arial")
S "Arial"
S "Arial"
S ((["a"; "b"; "c"; "d"], "Arial"), "root")
S (["c"; "b"; "a"; "root"], "d")
```

# Browse

+ [Signatures](https://github.com/corsis/XParsec/blob/master/XParsec.fsi#slider)
+ [Implementation](https://github.com/corsis/XParsec/blob/master/XParsec.fs#slider)

# License

```
Copyright (c) 2012, Cetin Sert

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

[![corsis]](https://github.com/corsis/)

[cetin@corsis.eu](mailto:fusion@corsis.eu)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"