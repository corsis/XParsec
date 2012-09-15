XParsec
=======

A stream-type-independent parsec implementation in F# 3.0.

---

# Example

```
open XParsec.Combinators
open XParsec.Xml

let r = next­ >. !@"fo­nt" |-> Strin­g.length .> "font­" @~? "Bold­" .>. next .> !@+ "s" <| AE.Ne­w [| X.Par­se "<span font=­'Bold' />"; X.Par­se "<span s='('­ />" |];;
```

```
val r : (int * X) Reply = S (4, <span s="(" />)
```

----

## Contact

[![corsis]](https://github.com/corsis/)

[fusion@corsis.eu](mailto:fusion@corsis.eu)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"