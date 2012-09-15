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

# Browse

+ [Signatures](https://github.com/corsis/XParsec/blob/master/XParsec.fsi)
+ [Implementation](https://github.com/corsis/XParsec/blob/master/XParsec.fs)

## Contact

[![corsis]](https://github.com/corsis/)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"