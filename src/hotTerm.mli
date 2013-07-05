(** In this module, terms of constructors (with types), and
  variables are implemented.
  
  @author: Robert Jakob
*)

(** Exception describing that a given path is not present
  in a term. *)
exception Path_not_found_in_term

(** The signature of a term module. *)
module type S = sig

  (** The type of the type of a constructor term. *)
  type atype

  (** The type of a term. *)
  type t = private
    | App of t * t list
    | Ctor of string * atype
    | Var of string
    | Bottom

  (** Creates an application term
    and eta reduces the term if possible. *)
  val mkApp : t -> t list -> t

  (** Creates a constructor term with a type. *)
  val mkCtor : string -> atype -> t

  (** Creates a variable. *)
  val mkVar : string -> t

  (** Creates the bottom value. *)
  val mkBottom : t

  (** Returns a human-readable representation of the given term.
    If parameter sort is set to true, then sorts are printed with
    all ctors. Default is false.
  *)
  val string_of : ?sort:bool -> t -> string

  (** Eta reduction of the given term. *)
  val eta_reduce : t -> t

  (** Replaces a variable in a term by a new term. Arguments are:
    - The variable to replace.
    - The term which should be inserted instead of the variable.
    - The term where the replacement should happen. *)
  val subst : string -> t -> t -> t

  (** Replace the subterm at path in term by newterm.
    For the path beeing empty, the newterm is returned instead of the input term.

    So for [subst_path p new t] we have:
    - [p] The path where the substitution should take place
    - [new] The newterm which should be inserted at [path]
    - [term] The term where the replacement should happen

    See "Baader, Franz et al. - Term Rewriting and All That". *)
  val subst_path : HotPath.t -> t -> t -> t

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
  val path : t -> string -> HotPath.t option

  (** Returns a subterm from a term at a given path.
    If the path is not valid in the given term,
    Path_not_found_in_term is raised. *)
  val read : t -> HotPath.t -> t

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
 
  (** Defines infix operators for some regularly used functions. *)
  module type Infix = sig

    (** Return path to variable. Infix variant of {!HotTerm.S.path}. *)
    val (-->) : t -> string -> HotPath.t option

    (** Read subterm at path. Infix variant of {!HotTerm.S.read}. *)
    val (-.) : t -> HotPath.t -> t
  end
end

module Make :
  functor (ASort : HotType.S) ->
    sig
      type atype = ASort.t
      type t = private
        | App of t * t list
        | Ctor of string * atype
        | Var of string
        | Bottom

      val mkApp : t -> t list -> t
      val mkCtor : string -> atype -> t
      val mkVar : string -> t
      val mkBottom : t

      val string_of : ?sort:bool -> t -> string  
      val eta_reduce : t -> t
      val subst : string -> t -> t -> t
      val subst_path : HotPath.t -> t -> t -> t
      val path : t -> string -> HotPath.t option
      val read : t -> HotPath.t -> t
      val is_welldefined : t -> bool
      val depth : t -> int

      module Infix : sig
        val (-->) : t -> string -> HotPath.t option
        val (-.) : t -> HotPath.t -> t
      end
    end

(** Sort term represents terms with their constructor types beeing sorts. *)
module SortTerm : sig
  type atype = HotType.Sort.t
  type t = private
    | App of t * t list
    | Ctor of string * atype
    | Var of string
    | Bottom

  val mkApp : t -> t list -> t
  val mkCtor : string -> atype -> t
  val mkVar : string -> t
  val mkBottom : t

  val string_of : ?sort:bool -> t -> string
  val eta_reduce : t -> t
  val subst : string -> t -> t -> t
  val subst_path : HotPath.t -> t -> t -> t
  val path : t -> string -> HotPath.t option
  val read : t -> HotPath.t -> t
  val is_welldefined : t -> bool
  val depth : t -> int

  module Infix : sig
    val (-->) : t -> string -> HotPath.t option
    val (-.) : t -> HotPath.t -> t
  end
end

(* TODO Create term module instance from a given RankedAlphabet's type? *)
