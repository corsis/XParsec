// Copyright (c) Cetin Sert 2012
// License: Simplified BSD.

module XParsec

open   System


  [<AutoOpen>]
  module Streams =

    open System.Collections
    open System.Collections.Generic

    type 'a ArrayEnumerator =
        member Current  : 'a
        member State    : int
        member State    : int with set
        member MoveNext : unit -> bool
        member MoveBack : unit -> bool
        member Copy     : unit -> 'a ArrayEnumerator
        new : a:'a [] * ?i:int -> 'a ArrayEnumerator
        static member inline New : a:'a [] -> 'a ArrayEnumerator
        interface IDisposable
        interface IEnumerator
        interface IEnumerator<'a>
    type 'a AE      = 'a ArrayEnumerator
    type 'a  E      = 'a     IEnumerator
    type 'a  S      = 'a      E
    type 'a  Stream = 'a      S


  type 'b_ Reply = S of 'b_ | F with
    member inline Value   : 'b_
    member inline IsMatch : bool
    static member inline FromBool :                          bool -> unit Reply
    static member inline Negate   :                      'a Reply -> unit Reply
    static member inline Put      :        'b         -> 'a Reply -> 'b   Reply
    static member inline Map      : ('a -> 'b)        -> 'a Reply -> 'b   Reply
    static member inline Choose   : ('a -> 'b option) -> 'a Reply -> 'b   Reply
  type 'b_ R = 'b_ Reply

  open Streams

  type Parser<'a,'b> = 'a Stream -> 'b Reply
  type      P<'a,'b> = 'a S      -> 'b R


  [<AutoOpen>]
  module Combinators =

    val inline Δ<'a>   :               'a
    val inline ( ?-> ) : bool -> 'a -> 'a option

    val inline preturn : 'b -> Parser<'a,'b>
    val inline pzero   :       Parser<'a,'b>

    val inline current :       Parser<'a,'a>
    val inline next    :       Parser<'a,'a>

    val inline attempt : Parser<'a,'b> ->                      Parser<'a,'b>        
    val inline negate  : Parser<'a,'b> ->                      Parser<'a,unit>
    val inline ( |-> ) : Parser<'a,'b> -> ('b -> 'c)        -> Parser<'a,'c>
    val inline ( |?> ) : Parser<'a,'b> -> ('b -> 'c option) -> Parser<'a,'c>

    val inline ( .>  ) : Parser<'a,'b> -> Parser<'a,'c> -> Parser<'a,'b>
    val inline (  >. ) : Parser<'a,'b> -> Parser<'a,'c> -> Parser<'a,'c>
    val inline ( .>. ) : Parser<'a,'b> -> Parser<'a,'c> -> Parser<'a,'b*'c>
    val inline ( </> ) : Parser<'a,'b> -> Parser<'a,'b> -> Parser<'a,'b>

    val inline many       :        Parser<'a,'b> -> Parser<'a,'b list>
    val inline many1      :        Parser<'a,'b> -> Parser<'a,'b list>
    val inline array      : int -> Parser<'a,'b> -> Parser<'a,'b []>
    val inline skipMany'  :        Parser<'a,'b> -> Parser<'a,int>
    val inline skipMany1' :        Parser<'a,'b> -> Parser<'a,int>
    val inline skipMany   :        Parser<'a,'b> -> Parser<'a,unit>
    val inline skipMany1  :        Parser<'a,'b> -> Parser<'a,unit>
    val inline skipN      : int -> Parser<'a,'b> -> Parser<'a,unit>

    val inline ( !* ) : Parser<'a,'b> -> Parser<'a,unit>
    val inline ( !+ ) : Parser<'a,'b> -> Parser<'a,unit>
  

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
      val inline ( !@   )  :    A ->           Parser<E,string>
      val inline ( !@~  )  :    A ->           Parser<E,unit>
      val inline ( !@+  )  :    A ->           Parser<E,unit>
      val inline (  @~? )  :    A -> string -> Parser<E,unit>
      val inline (  @~! )  :    A -> string -> Parser<E,unit>