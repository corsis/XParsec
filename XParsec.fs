// Copyright (c) Cetin Sert 2012
// License: Simplified BSD.

#if INTERACTIVE
#else
module XParsec
#endif

#if INTERACTIVE
#r "System.Xml.Linq.dll"
#endif

open System
open System.Collections.Generic

module Streams =

  type 'a ArrayEnumerator (a : 'a [], ?i : int) as e =
    let         l = a.Length
    let mutable s = -1 |> defaultArg i
    member e.Current           = a.[s]
    member e.Reset          () = s <- -1 |> defaultArg i
    member e.MoveNext       () = let i = s + 1 in if i <  l then s <- i; true else false
    member e.MoveBack       () = let i = s - 1 in if i > -1 then s <- i; true else false
    member e.State with get () = s and   set i =  if i <  l then s <- i       else raise <| ArgumentOutOfRangeException()
    member e.Copy           ()           = new ArrayEnumerator<_>(a, s)
    static member inline New (a : 'a []) = new ArrayEnumerator<_>(a)
    interface 'a IEnumerator with
      member i.Current     = e.Current
    interface Collections.IEnumerator with
      member i.Current     = e.Current :> obj
      member i.MoveNext () = e.MoveNext ()
      member i.Reset    () = e.Reset    ()
    interface IDisposable with
      member i.Dispose  () = ()

  type 'a IEnumerator with
    member inline e.Copy     () = (e :?> 'a ArrayEnumerator).Copy     ()
    member inline e.MoveBack () = (e :?> 'a ArrayEnumerator).MoveBack ()

  type 'a  E = 'a     IEnumerator
  type 'a AE = 'a ArrayEnumerator
  type 'a  S = 'a      E

  type 'a  Stream = 'a S

open Streams

type 'a Reply = S of 'a | F with
  member inline r.Value   = match r with S x -> x | F -> raise <| new InvalidOperationException()
  member inline r.IsMatch = match r with F -> false | S _ -> true 
  static member inline FromBool b = if b then S () else F
  static member inline Negate   r = match r with F -> S () | S _ -> F
  static member inline Put    x r = match r with F -> F    | S _ -> S      x
  static member inline Map    f r = match r with F -> F    | S x -> S <| f x
  static member inline Choose f r = match r with F -> F    | S x -> match f x with Some v -> S v | None -> F

type 'a R = 'a Reply

type      P<'a,'b> = 'a Stream -> 'b Reply
type Parser<'a,'b> = 'a Stream -> 'b Reply

module Combinators =

  let inline Δ<'a>         = Unchecked.defaultof<'a>
  let inline (?->) b x     = if b then Some x else None
  let inline back (s :_ S) = s.MoveBack() |> ignore

  let inline attempt (p : P<_,_>) (s : _ S) = s.Copy() |> p
  let inline negate  (p : P<_,_>)  s        = s        |> p |> Reply<_>.Negate

  let inline pzero     (_ : _ S) = S Δ
  let inline preturn x (_ : _ S) = S x

  let inline current   (e : _ S) = e.Current |> S
  let inline next      (e : _ S) = if e.MoveNext() then e |> current else F

  let inline (|->) (p : P<_,_>) f e = e |> p |> Reply<_>.Map    f
  let inline (|?>) (p : P<_,_>) f e = e |> p |> Reply<_>.Choose f
  let inline (.> ) (p : P<_,_>) (q : P<_,_>) e = match p e with F -> F   | S p -> q e |> Reply<_>.Put p
  let inline ( >.) (p : P<_,_>) (q : P<_,_>) e = match p e with F -> F   | S _ -> q e
  let inline (.>.) (p : P<_,_>) (q : P<_,_>) e = match p e with F -> F   | S p -> q e |> Reply<_>.Map (fun q -> (p,q))
  let inline (</>) (p : P<_,_>) (q : P<_,_>) e = match p e with F -> q e | s   -> s

  let inline many    (p : P<_,_>) (s : _ S) = let r = ref Δ in let q = Seq.toList <| seq { while (r := p s; (!r).IsMatch) do yield (!r).Value } in back s; S q
  let inline many1   (p : P<_,_>) (s : _ S) = s |> many p |> Reply<_>.Choose (function _::_ as l -> Some l | _ -> None)
  let inline array n (p : P<_,_>) (s : _ S) = s |> many p |> Reply<_>.Choose (function l -> let a = l |> List.toArray in (a.Length = n) ?-> a)

  let inline skipMany'  (p : P<_,_>) (s : _ S) = let c = ref 0 in (while (p s).IsMatch do c := !c + 1); back s; S !c
  let inline skipMany1' (p : P<_,_>) (s : _ S) = s |> skipMany'  p |> Reply<_>.Choose (fun n -> if n > 0 then Some n  else None)
  let inline skipMany   (p : P<_,_>) (s : _ S) = s |> skipMany'  p |> Reply<_>.Put ()
  let inline skipMany1  (p : P<_,_>) (s : _ S) = s |> skipMany1' p |> Reply<_>.Put ()
  let inline skipN   i  (p : P<_,_>) (s : _ S) = s |> skipMany'  p |> Reply<_>.Choose (fun n -> if n = i then Some () else None)

  let inline (!*) p s = skipMany  p s
  let inline (!+) p s = skipMany1 p s

  let inline (|?=) (p : P<_,_>) q = p |?> function (x1,y1),(x2,y2) -> if q x1 x2 then Some (y1,y2) else None
  let inline (|=?) (p : P<_,_>) q = p |?> function (x1,y1),(x2,y2) -> if q y1 y2 then Some (x1,x2) else None

module Xml =

  type E = System.Xml.Linq.XElement
  type A = string // Attribute Name

  module Operators =

    let inline (!>)  x   = ( ^a : (static member op_Implicit : ^b -> ^a) x )
    let inline (~~)  s   = s |> String.IsNullOrWhiteSpace
    let inline (-!-) a b = (a : string).Contains b |> not
    let inline (-?-) a b = (a : string).Contains b

    let inline (@ ) (e:E) a   = let a = e.Attribute(!> a) in if a <> null then a.Value else String.Empty
    let inline (@<) (e:E) a v = e.SetAttributeValue(!> a, v)
    let inline (@?)  e    a v =   (e @ a) -?- v
    let inline (@!)  e    a v =   (e @ a) -!- v
    let inline (@~)  e    a   = ~~(e @ a)

  module Parsers =

    open Operators

    let inline ( !@   ) a   (s : E S) = let x = s.Current.Attribute(!> a) in if x <> null then S x.Value else F
    let inline ( !@~  ) a   (s : E S) = (s.Current @~ a       ) |> Reply<_>.FromBool
    let inline ( !@+  ) a   (s : E S) = (s.Current @~ a |> not) |> Reply<_>.FromBool
    let inline (  @~? ) a v (s : E S) = (s.Current @? a <| v)   |> Reply<_>.FromBool
    let inline (  @~! ) a v (s : E S) = (s.Current @! a <| v)   |> Reply<_>.FromBool