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
let inline current (s:Source<'s,'a>) = s.Current


module Combinators =

  let inline (?->) b x = if b then Some x else None

  let inline current s = S <| current s,s

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
    let inline ( !@   ) a   s = let x = (current s : E).Attribute(!> a) in (if x <> null then S x.Value else F),s
    let inline ( !@~  ) a   s = let r = ( current s @~ a        |> Reply<_>.FromBool) in r,s
    let inline ( !@+  ) a   s = let r = ( current s @~ a |> not |> Reply<_>.FromBool) in r,s
    let inline (  @~? ) a v s = let r = ((current s @? a <| v)  |> Reply<_>.FromBool) in r,s
    let inline (  @~! ) a v s = let r = ((current s @! a <| v)  |> Reply<_>.FromBool) in r,s


  module Sources =

    type N = XNode
    let inline nextNode (e : N) = e.NextNode
    let inline prevNode (e : N) = e.PreviousNode
    let rec find<'a when 'a :> N> (f : N -> N) (n : N) =
      match f n with
      | :?'a as x -> x
      | null      -> Δ
      | node      -> find f node

    type XElement with
      member inline     e.NextElement : E = find nextNode e
      member inline e.PreviousElement : E = find prevNode e
      member inline           e.Child : E = try e.Elements() |> Seq.head with _ -> Δ

    let inline enter (e:E) = Source(e,e)

    let next   s = match (current s : E).NextElement     with null -> F,s | x -> S x,enter x
    let prev   s = match (current s : E).PreviousElement with null -> F,s | x -> S x,enter x
    let parent s = match (current s : E).Parent          with null -> F,s | x -> S x,enter x
    let child  s = match (current s : E).Child           with null -> F,s | x -> S x,enter x