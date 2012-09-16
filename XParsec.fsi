// Copyright (c) Cetin Sert 2012
// License: Simplified BSD.

module XParsec

open   System


val inline Δ<'a> : 'a

[<Struct>]
type Source<'s,'a> =
  val State   : 's
  val Current : 'a
  new : 's * 'a -> Source<'s,'a>

type Reply<'b_> = S of 'b_ | F with
  member inline Value   : 'b_
  member inline IsMatch : bool
  static member inline FromBool :                          bool -> unit Reply
  static member inline Negate   :                      'a Reply -> unit Reply
  static member inline Put      :        'b         -> 'a Reply -> 'b   Reply
  static member inline Map      : ('a -> 'b)        -> 'a Reply -> 'b   Reply
  static member inline Choose   : ('a -> 'b option) -> 'a Reply -> 'b   Reply

type Parser<'s,'a,'b> = Source<'s,'a> -> Reply<'b> * Source<'s,'a>

val inline reply   : 'b*_  -> 'b
val inline source  :  _*'s -> 's
val inline current : Source<_,'a> -> 'a

[<AutoOpen>]
module Combinators =

  val inline ( ?-> ) : bool -> 'a -> 'a option

  val inline current : Parser<'s,'a,'a>

  val inline attempt : Parser<'s,'a,'b> ->                      Parser<'s,'a,'b>
  val inline negate  : Parser<'s,'a,'b> ->                      Parser<'s,'a,unit>
  val inline ( |-> ) : Parser<'s,'a,'b> -> ('b -> 'c)        -> Parser<'s,'a,'c>
  val inline ( |?> ) : Parser<'s,'a,'b> -> ('b -> 'c option) -> Parser<'s,'a,'c>

  val inline ( .>  ) : Parser<'s,'a,'b> -> Parser<'s,'a,'c> -> Parser<'s,'a,'b>
  val inline (  >. ) : Parser<'s,'a,'b> -> Parser<'s,'a,'c> -> Parser<'s,'a,'c>
  val inline ( .>. ) : Parser<'s,'a,'b> -> Parser<'s,'a,'c> -> Parser<'s,'a,'b*'c>
  val inline ( </> ) : Parser<'s,'a,'b> -> Parser<'s,'a,'b> -> Parser<'s,'a,'b>

  val inline many       :        Parser<'s,'a,'b> -> Parser<'s,'a,'b list>
  val inline many1      :        Parser<'s,'a,'b> -> Parser<'s,'a,'b list>
  val inline array      : int -> Parser<'s,'a,'b> -> Parser<'s,'a,'b []>
  val inline skipMany'  :        Parser<'s,'a,'b> -> Parser<'s,'a,int>
  val inline skipMany1' :        Parser<'s,'a,'b> -> Parser<'s,'a,int>
  val inline skipN      : int -> Parser<'s,'a,'b> -> Parser<'s,'a,unit>
  val inline skipMany   :        Parser<'s,'a,'b> -> Parser<'s,'a,unit>
  val inline skipMany1  :        Parser<'s,'a,'b> -> Parser<'s,'a,unit>

  val inline ( !* )     :        Parser<'s,'a,'b> -> Parser<'s,'a,unit>
  val inline ( !+ )     :        Parser<'s,'a,'b> -> Parser<'s,'a,unit>
  

module Xml =

  type E = System.Xml.Linq.XElement
  type A = string // Attribute Name

  [<AutoOpen>]
  module Operators =
    val inline ( !> )  :  ^b -> ^a when ^a : (static member op_Implicit : ^b -> ^a)
    val inline ( ~~ )  : string ->           bool
    val inline ( -!- ) : string -> string -> bool
    val inline ( -?- ) : string -> string -> bool

    val inline ( @  )  : E -> A -> string
    val inline ( @< )  : E -> A -> 'a     -> unit
    val inline ( @? )  : E -> A -> string -> bool
    val inline ( @! )  : E -> A -> string -> bool
    val inline ( @~ )  : E -> A ->           bool

  [<AutoOpen>]
  module Parsers =
    val inline ( !@   )  :    A ->           Parser<'s, E,string>
    val inline ( !@~  )  :    A ->           Parser<'s,#E,unit>
    val inline ( !@+  )  :    A ->           Parser<'s,#E,unit>
    val inline (  @~? )  :    A -> string -> Parser<'s,#E,unit>
    val inline (  @~! )  :    A -> string -> Parser<'s,#E,unit>

  [<AutoOpen>]
  module Sources =
    val inline enter : E -> Source<E,E>

    val next   :      Parser<E,E,E>
    val prev   :      Parser<E,E,E>
    val parent :      Parser<E,E,E>
    val child  :      Parser<E,E,E>