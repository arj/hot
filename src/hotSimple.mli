(** Some simple concrete modules of the structures defined in this library.

  @author Robert Jakob
  *)


(** A string and sort based implementation of a ranked element. *)
module StringRankedAlphabet : HotRankedAlphabet.S with type elt = string * HotType.Sort.t


(** StringRankedAlphabet and Sort based terms. *)
module StringSortTerm :
  sig
    type re = StringRankedAlphabet.elt
    type t =
      HotTerm.Make(StringRankedAlphabet).t = private
        App of t * t list
      | Ctor of re
      | Var of string
      | Bottom
    val mkApp : t -> t list -> t
    val mkCtor : re -> t
    val mkVar : string -> t
    val mkBottom : t
    module Path :
      sig
        type termt = t
        type t =
          HotTerm.Make(StringRankedAlphabet).Path.t =
            Empty
          | Ele of re * int * t
        val epsilon : t
        val string_of : ?epsilon:bool -> t -> string
        val append : t -> t -> t
        val reverse : t -> t
        val is_empty : t -> bool
        val subst_path : t -> termt -> termt -> termt
        val path : termt -> string -> t option
        val read : termt -> t -> termt
        module Infix :
          sig
            val ( --> ) : termt -> string -> t option
            val ( -. ) : termt -> t -> termt
          end
      end
    val string_of : ?sort:bool -> t -> string
    val eta_reduce : t -> t
    val subst : string -> t -> t -> t
    val is_welldefined : t -> bool
    val depth : t -> int
  end

