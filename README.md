XParsec
=======

A stream-type-independent parsec implementation in F#

---

# Example

```f#
let e  = X.Parse "<span bbox=\"152.16 584.626 246.826 595.656\"  font=\"TimesNewRoman\"      size=\"9.96\" s=\" \" />"
let x  = X.Parse "<span bbox=\"63.2999 584.626 152.105 595.656\" font=\"TimesNewRoman,Bold\" size=\"9.96\" s=\"Sahra Wagenknecht \" />"
let y  = X.Parse "<span bbox=\"152.16 584.626 246.826 595.656\"  font=\"TimesNewRoman\"      size=\"9.96\" s=\"(DIE LINKE)  . . . . . . .\" />"
let z1 = X.Parse "<span bbox=\"152.16 584.626 246.826 595.656\"  font=\"TimesNewRoman\"      size=\"9.96\" s=\"(DIE\" />"
let z2 = X.Parse "<span bbox=\"152.16 584.626 246.826 595.656\"  font=\"TimesNewRoman\"      size=\"9.96\" s=\"LINKE)  . . . . . . .\" />"

```

```f#
let o = "s" @~? "("
let c = "s" @~? ")"

let name    : _ XP = next .>. ( "font" @~? "Bold" >. !@"bbox" |-> left .> !@+"s" )
let party t : _ XP = next .>. ( "font" @~! "Bold" >. !@"bbox" |-> left .> t      )

let sp2 : _ XP = name .->. party (o .> c)       |?> function  (x1,l1), (x2,l2)           -> (l1 < l2)            ?-> [x1; x2]
let sp3 : _ XP = name .->. party o .->. party c |?> function ((x1,l1), (x2,l2)), (x3,l3) -> (l1 < l2 && l1 < l3) ?-> [x1; x2; x3]


let ms xs = xs |> Seq.toArray |> ArrayEnumerator.New

let g = [x;e;e;z1;z2]  
let s = ms g in (s |> (name >. !@ "s" .> skipN 2 empty .>. party o .>. many empty .>. party c), s.State) |> printfn "%A"
```

---

```
(S ((("Sahra Wagenknecht ",
      (<span bbox="152.16 584.626 246.826 595.656" font="TimesNewRoman" size="9.96" s="(DIE" />,
       152.16)), []),
    (<span bbox="152.16 584.626 246.826 595.656" font="TimesNewRoman" size="9.96" s="LINKE)  . . . . . . ." />,
     152.16)), 4)
```