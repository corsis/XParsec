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


[<AutoOpen>]
module Combinators =

  val inline ( ?-> ) : bool -> 'a -> 'a option

  val inline current : Parser<'s,'a,'a>

  val inline future  : unit -> Parser<'s,'a,'b> * Parser<'s,'a,'b> ref
  val inline ahead   : Parser<'s,'a,'b> ->                      Parser<'s,'a,'b>
  val inline negate  : Parser<'s,'a,'b> ->                      Parser<'s,'a,unit>
  val inline ( => )  : Parser<'s,'a,'b> -> ('b -> 'c)        -> Parser<'s,'a,'c>
  val inline ( ?> )  : Parser<'s,'a,'b> -> ('b -> 'c option) -> Parser<'s,'a,'c>

  val inline ( .>  ) : Parser<'s,'a,'b> -> Parser<'s,'a,'c> -> Parser<'s,'a,'b>
  val inline (  >. ) : Parser<'s,'a,'b> -> Parser<'s,'a,'c> -> Parser<'s,'a,'c>
  val inline ( .>. ) : Parser<'s,'a,'b> -> Parser<'s,'a,'c> -> Parser<'s,'a,'b*'c>
  val inline ( </> ) : Parser<'s,'a,'b> -> Parser<'s,'a,'b> -> Parser<'s,'a,'b>

  val inline manyMax     : int -> Parser<'s,'a,'b> -> Parser<'s,'a,'b list>
  val inline many        :        Parser<'s,'a,'b> -> Parser<'s,'a,'b list>
  val inline many1       :        Parser<'s,'a,'b> -> Parser<'s,'a,'b list>
  val inline array       : int -> Parser<'s,'a,'b> -> Parser<'s,'a,'b []>
  val inline skipManyMax : int -> Parser<'s,'a,'b> -> Parser<'s,'a,int>
  val inline skipMany    :        Parser<'s,'a,'b> -> Parser<'s,'a,int>
  val inline skipMany1   :        Parser<'s,'a,'b> -> Parser<'s,'a,int>
  val inline skipN       : int -> Parser<'s,'a,'b> -> Parser<'s,'a,unit>

  val inline ( !*. )     :        Parser<'s,'a,'b> -> Parser<'s,'a,'b list>
  val inline ( !+. )     :        Parser<'s,'a,'b> -> Parser<'s,'a,'b list>
  val inline ( !*  )     :        Parser<'s,'a,'b> -> Parser<'s,'a,int>
  val inline ( !+  )     :        Parser<'s,'a,'b> -> Parser<'s,'a,int>
  

module Xml =

  open System.Xml.Linq

  type E = XElement
  type A = XAttribute
  type N = string
  type V = string


  [<AutoOpen>]
  module Operators =
    val inline ( !>  ) : ^b -> ^a when ^a : (static member op_Implicit : ^b -> ^a)
    val inline ( ~~  ) : string ->           bool
    val inline ( -?- ) : string -> string -> bool
    val inline ( -!- ) : string -> string -> bool

    val inline ( @@ )  : E -> N ->  A
    val inline ( @  )  : E -> N ->  V
    val inline ( @- )  : E -> N ->       bool
    val inline ( @+ )  : E -> N ->       bool
    val inline ( @? )  : E -> N ->  V -> bool
    val inline ( @! )  : E -> N ->  V -> bool

    val inline name   : ^x -> N when ^x : (member Name  : XName)
    val inline value  : ^x -> V when ^x : (member Value : V)


  [<AutoOpen>]
  module Parsers =
    val inline ( !<>  ) : N ->      Parser<'s,E,E>
    val inline ( !@@  ) : N ->      Parser<'s,E,A>
    val inline ( !@   ) : N ->      Parser<'s,E,V>
    val inline ( !@-  ) : N ->      Parser<'s,E,unit>
    val inline ( !@+  ) : N ->      Parser<'s,E,unit>
    val inline (  @~? ) : N -> V -> Parser<'s,E,unit>
    val inline (  @~! ) : N -> V -> Parser<'s,E,unit>

  [<AutoOpen>]
  module Navigation =
    type XElement with
      member             NextElement : E
      member         PreviousElement : E
      member inline            Child : E
      static member inline    source : E -> Source<E,E>
    val next   :                            Parser<E,E,E>
    val prev   :                            Parser<E,E,E>
    val parent :                            Parser<E,E,E>
    val child  :                            Parser<E,E,E>
    [<AutoOpen>]
    module Parsers =
      val inline children : Parser<E,E,'b> -> Parser<E,E,'b list>