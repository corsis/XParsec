XParsec
=======

A stream-type-independent parsec implementation in F# 3.0.

---

# Example 1

```
open XParsec.Combinators
open XParsec.Xml

let r = next >. !@"fo­nt" .> "font­" @~? "Bold­" .>. next .> !@+ "s" <| AE.Ne­w [| X.Par­se "<span font=­'Bold' />"; X.Par­se "<span s='('­ />" |];;
```

```
val r : (string * X) Reply = S (<span font="Bold" />, <span s="(" />)
```

----

## Contact

[![corsis]](https://github.com/corsis/)

[fusion@corsis.eu](mailto:fusion@corsis.eu)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"