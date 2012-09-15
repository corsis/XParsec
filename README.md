# Example

```
open XParsec.Combinators
open XParsec.Xml

let stream = AE.New [| X.Parse "<span font='Bold' />"; X.Parse "<span s='(' />" |]
let reply  = next >. !@"font" |-> String.length .> "font" @~? "B" .>. next .> !@+ "s" <| stream
```

```
val reply : (int * X) Reply = S (4, <span s="(" />)
```

---

[![corsis]](https://github.com/corsis/)

[fusion@corsis.eu](mailto:fusion@corsis.eu)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"