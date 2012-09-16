**XParsec** can work with **any type**, supports **[domain-specific non-linear navigation](https://github.com/corsis/XParsec/blob/16de327b98410e3031636ffa86572e12d52f4594/XParsec.fsi#L88)** and is [**very easy to extend**](https://github.com/corsis/XParsec/blob/f6bc93499a588b287dc20f622fb917e1edac58b8/XParsec.fs#L111).

(FParsec only works with `Char`s and can only go forward on a one dimensional string.)

# Example

Below we use [`XParsec.Xml`](https://github.com/corsis/XParsec/blob/c79ce4ef4ec7401d1c0db8347fbfce6837078e44/XParsec.fsi#L61) – the first XParsec extension [implemented in just 19 lines of F#](https://github.com/corsis/XParsec/blob/c79ce4ef4ec7401d1c0db8347fbfce6837078e44/XParsec.fs#L102).

```fsharp
  open XParsec
  open XParsec.Xml

  let show reply = printfn "%A" reply
  let test parse = show << reply << parse << enter
  let name (e:E) = string e.Name
  //          ^
  //         XElement

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

  test parser1 root; test parser2 root; test parser3 root
```
```fsharp
S (["a"; "b"; "c"; "d"], "Arial")
S "Arial"
S "Arial"
```

# Browse

+ [Signatures](https://github.com/corsis/XParsec/blob/master/XParsec.fsi)
+ [Implementation](https://github.com/corsis/XParsec/blob/master/XParsec.fs)

# Contact

[![corsis]](https://github.com/corsis/)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"