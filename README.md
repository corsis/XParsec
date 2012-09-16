**XParsec** works with [**any type**](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fsi#L26), supports **[domain-specific non-linear navigation](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fsi#L88)** and is [**very easy to extend**](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fs#L102).

(FParsec only works with `Char`s and can only go forward on a one dimensional string.)

# Example

Below we use [`XParsec.Xml`](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fsi#L61) which provides the first XParsec extension [implemented in just 19 lines of F#](https://github.com/corsis/XParsec/blob/0284b134a566ad2470d39a71fb94d7f4cbac0bdb/XParsec.fs#L102).

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

  // graceful look-ahead
  let parser4 = attempt parser1 .>. (current => name)

  test parser1 root; test parser2 root; test parser3 root; test parser4 root
```
```fsharp
S (["a"; "b"; "c"; "d"], "Arial")
S "Arial"
S "Arial"
S ((["a"; "b"; "c"; "d"], "Arial"), "root")
```

# Browse

+ [Signatures](https://github.com/corsis/XParsec/blob/master/XParsec.fsi)
+ [Implementation](https://github.com/corsis/XParsec/blob/master/XParsec.fs)

# Contact

[![corsis]](https://github.com/corsis/)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"