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


let inline Δ<'a> = Unchecked.defaultof<'a>

[<Struct>]
type Source<'s,'a> =
  val State   : 's
  val Current : 'a
  new (s : 's, c : 'a) = { State = s; Current = c }

type 'a Reply = S of 'a | F with
  member inline r.Value   = match r with S x -> x | F -> raise <| new InvalidOperationException()
  member inline r.IsMatch = match r with F -> false | S _ -> true 
  static member inline FromBool b = if b then S () else F
  static member inline Negate   r = match r with F -> S () | S _ -> F
  static member inline Put    x r = match r with F -> F    | S _ -> S      x
  static member inline Map    f r = match r with F -> F    | S x -> S <| f x
  static member inline Choose f r = match r with F -> F    | S x -> match f x with Some v -> S v | None -> F

type Parser<'s,'a,'b> = Source<'s,'a> -> Reply<'b> * Source<'s,'a>

let inline reply   (r,_) = r
let inline source  (_,s) = s


module Combinators =

  let inline (?->) b x = if b then Some x else None

  let inline current (s : Source<_,_>) = S <| s.Current,s

  let inline attempt (p : Parser<_,_,_>)   (s : Source<_,_>) = let r,_ = p s in r,s
  let inline negate  (p : Parser<_,_,_>)    s                = let r,s = p s in Reply<_>.Negate   r,s
  let inline (|->)   (p : Parser<_,_,_>) f  s                = let r,s = p s in Reply<_>.Map    f r,s
  let inline (|?>)   (p : Parser<_,_,_>) f  s                = let r,s = p s in Reply<_>.Choose f r,s

  let inline (.> ) (p : Parser<_,_,_>) (q : Parser<_,_,_>) s = let r,s = p s in match r with F -> F,s | S p -> let r,s = q s in Reply<_>.Put p r,s
  let inline ( >.) (p : Parser<_,_,_>) (q : Parser<_,_,_>) s = let r,s = p s in match r with F -> F,s | S _ -> q s
  let inline (.>.) (p : Parser<_,_,_>) (q : Parser<_,_,_>) s = let r,s = p s in match r with F -> F,s | S p -> let r,s = q s in Reply<_>.Map (fun q -> (p,q)) r,s
  let inline (</>) (p : Parser<_,_,_>) (q : Parser<_,_,_>) s = let r,s = p s in match r with F -> q s | p   -> p,s

  let inline many       (p : Parser<_,_,_>) s =
    let b = ref    Δ
    let l = ref (Δ,s)
    let q = Seq.toList <| seq { while (b := source !l; l := p !b; (reply !l).IsMatch) do yield (reply !l).Value }
    S q,!b
  let inline many1      (p : Parser<_,_,_>) s = let r,s = s |> many p in Reply<_>.Choose (function _::_ as l -> Some l | _ -> None)                        r,s
  let inline array n    (p : Parser<_,_,_>) s = let r,s = s |> many p in Reply<_>.Choose (function l -> let a = l |> List.toArray in (a.Length = n) ?-> a) r,s

  let inline skipMany'  (p : Parser<_,_,_>) s =
    let b = ref    Δ
    let l = ref (Δ,s)
    let c = ref  0
    let q = Seq.toList <| seq { while (b := source !l; l := p !b; (reply !l).IsMatch) do c := !c + 1 }
    S !c,!b
  let inline skipMany1' (p : Parser<_,_,_>) s = let r,s = s |> skipMany'  p in Reply<_>.Choose (fun n -> if n > 0 then Some n  else None) r,s
  let inline skipN   i  (p : Parser<_,_,_>) s = let r,s = s |> skipMany'  p in Reply<_>.Choose (fun n -> if n = i then Some () else None) r,s
  let inline skipMany   (p : Parser<_,_,_>) s = let r,s = s |> skipMany'  p in Reply<_>.Put    ()                                         r,s
  let inline skipMany1  (p : Parser<_,_,_>) s = let r,s = s |> skipMany1' p in Reply<_>.Put    ()                                         r,s

  let inline (!*) p s = skipMany  p s
  let inline (!+) p s = skipMany1 p s


module Xml =

  open System.Xml.Linq

  type E = XElement
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

  open Operators
    
          
  module Parsers =
    let inline ( !@   ) a   (s : Source<_,E>) = let x = ( s.Current : E ).Attribute(!> a) in (if x <> null then S x.Value else F),s
    let inline ( !@~  ) a   (s : Source<_,E>) = let r = ( s.Current @~ a        |> Reply<_>.FromBool) in r,s
    let inline ( !@+  ) a   (s : Source<_,E>) = let r = ( s.Current @~ a |> not |> Reply<_>.FromBool) in r,s
    let inline (  @~? ) a v (s : Source<_,E>) = let r = ((s.Current @? a <| v)  |> Reply<_>.FromBool) in r,s
    let inline (  @~! ) a v (s : Source<_,E>) = let r = ((s.Current @! a <| v)  |> Reply<_>.FromBool) in r,s


  module Sources =

    type N = XNode
    let inline next' (n:N) = let mutable c = Δ in (while (c <- n.NextNode;     match c with :? E as e -> true | _ -> false) do ()); match c with :? E as e -> e | _ -> Δ
    let inline prev' (n:N) = let mutable c = Δ in (while (c <- n.PreviousNode; match c with :? E as e -> true | _ -> false) do ()); match c with :? E as e -> e | _ -> Δ

    type XElement with
      member inline     e.NextElement = next' e
      member inline e.PreviousElement = prev' e
      member inline           e.Child = if e.HasElements then e.Elements() |> Seq.head else Δ
      member inline          e.Source = Source(e,e)

    let inline enter (e:E) = e.Source

    let next   (s : Source<_,E>) = match s.Current.NextElement     with null -> F,s | x -> S x,enter x
    let prev   (s : Source<_,E>) = match s.Current.PreviousElement with null -> F,s | x -> S x,enter x
    let parent (s : Source<_,E>) = match s.Current.Parent          with null -> F,s | x -> S x,enter x
    let child  (s : Source<_,E>) = match s.Current.Child           with null -> F,s | x -> S x,enter x