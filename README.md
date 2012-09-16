**XParsec** can work with **any type**, supports **[domain-specific non-linear navigation](https://github.com/corsis/XParsec/blob/16de327b98410e3031636ffa86572e12d52f4594/XParsec.fsi#L88)** and is [**very easy to extend**](https://github.com/corsis/XParsec/blob/f6bc93499a588b287dc20f622fb917e1edac58b8/XParsec.fs#L111).

(FParsec only works with `Char`s and can only go forward on a one dimensional string.)

# Example

Below we use [`XParsec.Xml`](https://github.com/corsis/XParsec/blob/c79ce4ef4ec7401d1c0db8347fbfce6837078e44/XParsec.fsi#L61) – the first XParsec extension [implemented in just 19 lines of F#](https://github.com/corsis/XParsec/blob/c79ce4ef4ec7401d1c0db8347fbfce6837078e44/XParsec.fs#L102).

```fsharp
  open XParsec
  open XParsec.Xml

  let show x          = printfn "%A" x
  let test root parse = root |> enter |> parse |> show
  let name (e : E)    = string e.Name
  //            ^
  //           XElement

  let root1 = E.Parse "<root><a><b><c><d font='Arial'></d></c></b></a></root>"

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

  test root1 parser1; test root1 parser2; test root1 parser3
```
```fsharp
(S (["a"; "b"; "c"; "d"], "Arial"), XParsec+Source`2[XElement,XElement])
(S "Arial", XParsec+Source`2[XElement,XElement])
(S "Arial", XParsec+Source`2[XElement,XElement])
```

# Browse

+ [Signatures](https://github.com/corsis/XParsec/blob/master/XParsec.fsi)
+ [Implementation](https://github.com/corsis/XParsec/blob/master/XParsec.fs)

# Contact

[![corsis]](https://github.com/corsis/)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"