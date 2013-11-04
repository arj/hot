(** In this module, terms of constructors (with types), and
  variables are implemented.

  @author: Robert Jakob
  *)

(** Exception describing that a given path is not present
  in a term. *)
exception Path_not_found_in_term

(** The signature of a term module. *)
module type S = sig

  (** The type of a constructor (ranked element), typically an element of a
    ranked alphabet. *)
  type re

  (** The type of a term. *)
  type t = private
    | App of t * t list
    | Ctor of re
    | Var of string
    | Bottom

  (** Creates an application term
    and eta reduces the term if possible. *)
  val mkApp : t -> t list -> t

  (** Creates a constructor term with a type. *)
  val mkCtor : re -> t

  (** Creates a variable. *)
  val mkVar : string -> t

  (** Creates the bottom value. *)
  val mkBottom : t

  (** Creates a ctor application directly. *)
  val mkAppCtor : re -> t list -> t

  (** This module provides paths based on the terms just defined.

    Typically, a path is given as pairs of constructor and index, i.e.
    [C.1,A.2,E.5] means a path where the first ctor is [C], then, at 1st position
    there is a [A], at 2nd position a [E] and this path finally points to the 5th
    position inside [E], then.

    @author Robert Jakob
    *)
  module Path : sig
    type termt = t
    (** Internal path representation. *)
    type t =
      | Empty (* The empty path. *)
      | Ele of re * int * t (* A path element consisting of a ctor, position and
                             another path. *)

    (** Epsilon represents the empty path. *)
    val epsilon : t

    (** Returns a string representation for the given path.
      When the optional parameter [?epsilon] is false (default), then
      an empty path results in the empty string, and a path [C.1] results
      in ["C.1"].
      With [?epsilon] beeing true, the empty path results in the string
      ["{epsilon}"].
      *)
    val string_of : ?epsilon:bool -> t -> string

    (** Appends a path to a path. *)
    val append : t -> t -> t

    (** Reverses a path. *)
    val reverse : t -> t

    (** Checks if the given path is empty. *)
    val is_empty : t -> bool

    (** Replace the subterm at path in term by newterm.
      For the path beeing empty, the newterm is returned instead of the input term.

      So for [subst_path p new t] we have:
      - [p] The path where the substitution should take place
      - [new] The newterm which should be inserted at [path]
      - [term] The term where the replacement should happen

      See "Baader, Franz et al. - Term Rewriting and All That". *)
    val subst_path : t -> termt -> termt -> termt

    (** Return the path to a variable in a term.
      Requires, that the variable is used only once,
      otherwise the response is undetermined.
      If the variable does not occur in the given term, [None]
      is returned.

      So for [path term var] we have
      - [term] The term where to look for a variable.
      - [var] The variable to look for.

      Whenever an application with first place variables occur,
      the result is [None], because no path can be determined for
      variables!
      *)
    val path : termt -> string -> t option

    (** Returns a subterm from a term at a given path.
      If the path is not valid in the given term,
      Path_not_found_in_term is raised. *)
    val read : termt -> t -> termt

    (** Returns the length of the path. {!Empty} is 0. *)
    val length : t -> int

    (** Removes a given suffix from a term if the
      suffix exists, otherwise raises Path_not_found_in_term. *)
    val remove_suffix : t -> t -> t

    (** Checks, if a path p has a suffix p', i.e.
      [is_suffix path suffix]. *)
    val is_suffix : t -> t -> bool

    (** Defines infix operators for some regularly used functions. *)
    module Infix : sig

      (** Return path to variable. Infix variant of {!HotTerm.S.Path.path}. *)
      val (-->) : termt -> string -> t option

      (** Read subterm at path. Infix variant of {!HotTerm.S.Path.read}. *)
      val (-.) : termt -> t -> termt
    end
  end

  (** Returns a human-readable representation of the given term.
    If parameter show_type is set to true, then types are printed with
    all ctors. Default is false.
    *)
  val string_of : ?show_type:bool -> t -> string

  (** Eta reduction of the given term. *)
  val eta_reduce : t -> t

  (** Replaces a variable in a term by a new term. Arguments are:
    - The variable to replace.
    - The term which should be inserted instead of the variable.
    - The term where the replacement should happen. *)
  val subst : string -> t -> t -> t

  (** Checks if the given term is well-defined.

    A term is well-defined if an application has
    no bottom at the head, and if it is a ctor, then
    the number of arguments is less or equal the type
    (due to partial application). Additionally, the sort
    matches, too.
    *)
  val is_welldefined : t -> bool

  ;;
  (** Returns the maximal depth of the given term.
    Atoms like [Bottom] and [Var] have depth 0.
    *)
  val depth : t -> int

  (** Compares two terms. *)
  val compare : t -> t -> int

  (** Extracts the paths to all variables that occur in the given term. *)
  val vars : t -> (string * Path.t) list

  (** Unify two given terms if possible. 
    Returns either a substitution [Ok(subst)] if unifiable,
    or [Err(())] if not.
  *)
  val unify : t -> t -> ((string * t) list, unit) BatResult.t 
end

(* TODO Write this using S! *)
module Make :
  functor (RA : HotRankedAlphabet.S) ->
sig
  type re = RA.elt
  type t = private
    | App of t * t list
    | Ctor of re
    | Var of string
    | Bottom

  val mkApp : t -> t list -> t
  val mkCtor : re -> t
  val mkVar : string -> t
  val mkBottom : t
  val mkAppCtor : re -> t list -> t

  module Path : sig
    type termt = t
    type t =
      | Empty
      | Ele of re * int * t

    val epsilon : t

    val string_of : ?epsilon:bool -> t -> string

    val append : t -> t -> t

    val reverse : t -> t

    val is_empty : t -> bool

    val subst_path : t -> termt -> termt -> termt
    val path : termt -> string -> t option
    val read : termt -> t -> termt
    val length : t -> int
    val remove_suffix : t -> t -> t
    val is_suffix : t -> t -> bool

    module Infix : sig
      val (-->) : termt -> string -> t option
      val (-.) : termt -> t -> termt
    end
  end

  val string_of : ?show_type:bool -> t -> string  
  val eta_reduce : t -> t
  val subst : string -> t -> t -> t
  val is_welldefined : t -> bool
  val depth : t -> int
  val compare : t -> t -> int
  val vars : t -> (string * Path.t) list
  val unify : t -> t -> ((string * t) list, unit) BatResult.t 
end
