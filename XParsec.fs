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


let inline Δ<'a> = Unchecked.defaultof<'a>

[<Struct>]
type Source<'s,'a> =
  val State   : 's
  val Current : 'a
  new (s : 's, c : 'a) = { State = s; Current = c }
  override s.ToString () = String.Format("Source({0},{1})", s.State, s.Current)

type 'a Reply = S of 'a | F | Q with
  member inline r.Value   = match r with S x -> x | Q | F -> raise <| new InvalidOperationException()
  member inline r.IsMatch = match r with Q | F -> false | S _ -> true 
  static member inline fromBool b = if b then S () else F
  static member inline negate   r = match r with Q -> Q | F -> S () | S _ -> F
  static member inline put    x r = match r with Q -> Q | F -> F    | S _ -> S      x
  static member inline map    f r = match r with Q -> Q | F -> F    | S x -> S <| f x
  static member inline choose f r = match r with Q -> Q | F -> F    | S x -> match f x with Some v -> S v | None -> F
  static member inline toOption r = match r with      Q | F -> None | S x -> Some x

type Parser<'s,'a,'b> = Source<'s,'a> -> Reply<'b> * Source<'s,'a>

let inline reply  (r,_) = r
let inline source (_,s) = s


module Combinators =

  let inline (?->) b x = if b then Some x else None

  let inline current (s : Source<_,_>) = S <| s.Current,s

  let inline future  () = let r = ref Δ in (fun s-> !r s), r : Parser<_,_,_> * Parser<_,_,_> ref
  let inline ahead   (p : Parser<_,_,_>)   s = let r,_ = p s in r,s
  let inline negate  (p : Parser<_,_,_>)   s = let r,s = p s in Reply<_>.negate   r,s
  let inline (=>)    (p : Parser<_,_,_>) f s = let r,s = p s in Reply<_>.map    f r,s
  let inline (?>)    (p : Parser<_,_,_>) f s = let r,s = p s in Reply<_>.choose f r,s

  let inline (.> ) (p : Parser<_,_,_>) (q : Parser<_,_,_>) s = let r,s = p s in match r with Q -> Q,s | F -> F,s | S p -> let r,s = q s in Reply<_>.put p r,s
  let inline ( >.) (p : Parser<_,_,_>) (q : Parser<_,_,_>) s = let r,s = p s in match r with Q -> Q,s | F -> F,s | S _ -> q s
  let inline (.>.) (p : Parser<_,_,_>) (q : Parser<_,_,_>) s = let r,s = p s in match r with Q -> Q,s | F -> F,s | S p -> let r,s = q s in Reply<_>.map (fun q -> (p,q)) r,s
  let inline (</>) (p : Parser<_,_,_>) (q : Parser<_,_,_>) s = let r,s = p s in match r with Q | F -> q s | p   -> p,s

  let inline manyMax     n (p : Parser<_,_,_>) s =
    let b = ref    Δ
    let l = ref (Δ,s)
    let c = ref  0
    let q = Seq.toList <| seq { while (b := source !l; l := p !b; !c < n && (reply !l).IsMatch) do c := !c + 1; yield (reply !l).Value }
    S q,!b
  let inline many          (p : Parser<_,_,_>) s = manyMax Int32.MaxValue p s
  let inline many1         (p : Parser<_,_,_>) s = let r,s = many      p s in Reply<_>.choose (function _::_ as l -> Some l | _ -> None) r,s
  let inline array       n (p : Parser<_,_,_>) s = let r,s = manyMax n p s in Reply<_>.choose (function         l -> let a = List.toArray l in (a.Length = n) ?-> a) r,s
  let inline skipManyMax n (p : Parser<_,_,_>) s =
    let mutable b =    Δ
    let mutable l = (Δ,s)
    let mutable c =  0
    while (b <- source l; l <- p b; c < n && (reply l).IsMatch) do c <- c + 1
    S c,b
  let inline skipMany      (p : Parser<_,_,_>) s = skipManyMax Int32.MaxValue p s
  let inline skipMany1     (p : Parser<_,_,_>) s = let r,s = s |> skipMany p in Reply<_>.choose (fun n -> if n > 0 then Some n  else None) r,s
  let inline skipN       x (p : Parser<_,_,_>) s = let r,s = s |> skipMany p in Reply<_>.choose (fun n -> if n = x then Some () else None) r,s

  let inline (!*.) p s =     many  p s
  let inline (!+.) p s =     many1 p s
  let inline (!*)  p s = skipMany  p s
  let inline (!+)  p s = skipMany1 p s

  let inline (>>=) (p : Parser<'s,'a,'b>) f s = let r,s = p s in match r with Q -> Q,s | F -> F,s | S b -> f b s


module Xml =

  open System.Xml.Linq

  type E = XElement
  type A = XAttribute
  type N = string
  type V = string


  module Operators =

    let inline (!>)  x   = ( ^a : (static member op_Implicit : ^b -> ^a) x )
    let inline (~~)  a   = a |>           String.IsNullOrWhiteSpace
    let inline (-?-) a b = match (a : string) with null -> false | _ -> a.Contains b
    let inline (-!-) a b = match (a : string) with null -> true  | _ -> a.Contains b |> not

    let inline (@@) (e:E) n   =       e.Attribute(!> n)
    let inline (@ ) (e:E) n   = match e.Attribute(!> n) with null -> Δ | a -> a.Value
    let inline (@-)  e    n   = ~~(e @ n)
    let inline (@+)  e    n   = ~~(e @ n) |> not
    let inline (@?)  e    n v =   (e @ n) -?- v
    let inline (@!)  e    n v =   (e @ n) -!- v

    let inline name  x = ( ^a : (member Name  : XName ) x ).LocalName
    let inline value x = ( ^a : (member Value : string) x )

  open Operators
    
        
  module Parsers =

    let inline ( !<>  ) n   (s : Source<_,E>) = (match s.Current.Name.LocalName = n  with false -> F | _ -> S s.Current),s
    let inline ( !@@  ) n   (s : Source<_,E>) = (match s.Current.Attribute    (!> n) with null  -> F | a -> S a        ),s
    let inline ( !@   ) n   (s : Source<_,E>) = (match s.Current.Attribute    (!> n) with null  -> F | a -> S a.Value  ),s
    let inline ( !@-  ) n   (s : Source<_,E>) = ( s.Current @- n       |> Reply<_>.fromBool),s
    let inline ( !@+  ) n   (s : Source<_,E>) = ( s.Current @+ n       |> Reply<_>.fromBool),s
    let inline (  @~? ) n v (s : Source<_,E>) = ((s.Current @? n <| v) |> Reply<_>.fromBool),s
    let inline (  @~! ) n v (s : Source<_,E>) = ((s.Current @! n <| v) |> Reply<_>.fromBool),s


  module Navigation =

    type N = XNode

    type XElement with
      member            e.NextElement = let n = e :> N in let mutable c = n in (while (c <- c.NextNode;     match c with :? E | null -> false | _ -> true) do ()); match c with :? E as e -> e | _ -> Δ
      member        e.PreviousElement = let n = e :> N in let mutable c = n in (while (c <- c.PreviousNode; match c with :? E | null -> false | _ -> true) do ()); match c with :? E as e -> e | _ -> Δ
      member inline           e.Child = if e.HasElements then e.Elements() |> Seq.head else Δ
      static member inline   source e = Source((e:E),e)

    let next   (s : Source<_,E>) = match s.Current.NextElement     with null -> Q,s | x -> S x,E.source x
    let prev   (s : Source<_,E>) = match s.Current.PreviousElement with null -> Q,s | x -> S x,E.source x
    let parent (s : Source<_,E>) = match s.Current.Parent          with null -> Q,s | x -> S x,E.source x
    let child  (s : Source<_,E>) = match s.Current.Child           with null -> Q,s | x -> S x,E.source x

    module Parsers =
      open Combinators
      let inline children p = ahead (child>.p  .>.  many (next>.p)) => function c,cs -> c::cs


module Array =

  type  Position = Int32
  type 'a Stream = Source<'a [], Position>
  let        pre = Position.MinValue
  let       post = Position.MaxValue

  let inline clamp l u n = l |> max <| n |> min <| u
  let inline (|?|)  a  i = i > - 1 && i < (a:_[]).Length

  module Array =
    let inline source i (s : _ seq) = let a = Seq.toArray s in let i = clamp -1 a.Length i in Source(Source(a, i), if a |?| i then a.[i] else Δ)

  module Navigation =
    type  Σ<'s,'a>  = Source<'s,'a>
    let inline σ (s : Source< _, _>) = s.State
    let inline χ (s : Source< _, _>) = s.Current
    let inline next s = let a : _ [] = σ (σ s) in let c = χ (σ s) + 1 in match c < a.Length with false -> Q,s | true -> S a.[c],Σ(Σ(a,c),a.[c])
    let inline prev s = let a : _ [] = σ (σ s) in let c = χ (σ s) - 1 in match c > -1       with false -> Q,s | true -> S a.[c],Σ(Σ(a,c),a.[c])