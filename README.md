XParsec
=======

A stream-type-independent parsec implementation in F#

---

# Example

```
let e  = X.Parse "<span bbox=\"152.16 584.626 246.826 595.656\"  font=\"TimesNewRoman\"      size=\"9.96\" s=\" \" />"
let x  = X.Parse "<span bbox=\"63.2999 584.626 152.105 595.656\" font=\"TimesNewRoman,Bold\" size=\"9.96\" s=\"Sahra Wagenknecht \" />"
let y  = X.Parse "<span bbox=\"152.16 584.626 246.826 595.656\"  font=\"TimesNewRoman\"      size=\"9.96\" s=\"(DIE LINKE)  . . . . . . .\" />"
let z1 = X.Parse "<span bbox=\"152.16 584.626 246.826 595.656\"  font=\"TimesNewRoman\"      size=\"9.96\" s=\"(DIE\" />"
let z2 = X.Parse "<span bbox=\"152.16 584.626 246.826 595.656\"  font=\"TimesNewRoman\"      size=\"9.96\" s=\"LINKE)  . . . . . . .\" />"

```

```
open XParsec
open XParsec.Streams
open XParsec.Combinators
open XParsec.Xml

type XP<'b> = Parser<X,'b>


let inline left x = (s:string).Split [| ' ' |] |> Seq.head |> float

let empty : _ XP      = next .> !@~ "s"
let inline (.->.) p q = p .> !* empty .>. q


let o       : _ XP = "s" @~? "("
let c       : _ XP = "s" @~? ")"
let name    : _ XP = next .>. ( "font" @~? "Bold" >. !@"bbox" |-> left .> !@+"s" )
let party t : _ XP = next .>. ( "font" @~! "Bold" >. !@"bbox" |-> left .> t      )

let sp2 : _ XP = name .->. party (o .> c)       |?> function  (x1,l1), (x2,l2)           -> (l1 < l2)            ?-> [x1; x2]
let sp3 : _ XP = name .->. party o .->. party c |?> function ((x1,l1), (x2,l2)), (x3,l3) -> (l1 < l2 && l1 < l3) ?-> [x1; x2; x3]

let speaker : _ XP = (attempt sp2) </> sp3


let s = ArrayEnumerator.New [|x;e;e;z1;z2|] in printfn "%A" (s |> speaker, s.State)
```

---

```
(S [<span bbox="63.2999 584.626 152.105 595.656" font="TimesNewRoman,Bold" size="9.96" s="Sahra Wagenknecht " />;
    <span bbox="152.16 584.626 246.826 595.656" font="TimesNewRoman" size="9.96" s="(DIE" />;
    <span bbox="152.16 584.626 246.826 595.656" font="TimesNewRoman" size="9.96" s="LINKE)  . . . . . . ." />],
 4)
```