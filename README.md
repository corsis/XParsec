# Example

```
  open XParsec
  open XParsec.Xml // just one source provider, you can implement your own!

  let show x          = printfn "%A" x
  let test root parse = root |> enter |> parse |> show
  let name (e : E)    = string e.Name // easy to extend

  //

  let root1 = E.Parse "<root><a><b><c><d font='Arial'></d></c></b></a></root>"

  //            navigation can be
  //            domain-specific
  //                  v
  let parser1 = many (child|->name) >. !@"font"
  //            ^             ^
  //       packed with      easy to
  //       lots of          extend
  //       combinators

  test root1 parser1

  //

  let parser2 = parent|->name </> parser1

  test root1 parser2
```

```
(S "DeepDescent", XParsec+Source`2[XElement,XElement])
(S "DeepDescent", XParsec+Source`2[XElement,XElement])
```

# Browse

+ [Signatures](https://github.com/corsis/XParsec/blob/master/XParsec.fsi)
+ [Implementation](https://github.com/corsis/XParsec/blob/master/XParsec.fs)

# Contact

[![corsis]](https://github.com/corsis/)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"