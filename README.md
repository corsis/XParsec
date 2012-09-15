# Example

```
open XParsec
open XParsec.Xml

let s = AE.New [| E.Parse "<span font='Bold' />"; E.Parse "<span t='(' />" |]
let r = next >. !@"font"|->String.length .> "font"@~?"B" .>. next .> !@+"t" <| s
```

```
val r : (int * E) Reply = S (4, <span t="(" />)
```

# Browse

+ [Signatures](https://github.com/corsis/XParsec/blob/master/XParsec.fsi)
+ [Implementation](https://github.com/corsis/XParsec/blob/master/XParsec.fs)

## Contact

[![corsis]](https://github.com/corsis/)

[corsis]: http://portfusion.sourceforge.net/i/l100.png "Corsis Research"