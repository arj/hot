(** In this module, a higher-order recursion scheme (HORS) is defined.
  For reference, please see e.g. Kobayashi, Naoki - Higher-Order Model Checking:
  From Theory to Practice.
  
  @author Robert Jakob
*)

(** Exception for step on a non-terminal. *)
exception Path_does_not_point_to_nonterminal_in_app

(** Exception for {step_all_until_terminal} if the
  HORS is not productive. *)
exception HORS_is_not_productive

(** This module implements the set of rules for a HORS.
  A rule is a 3-tuple [(F,xs,t)] where [F] is a non-terminal,
  [xs] is a list of variables with [length xs = arity(F)] and
  t is a term of terminals, nonterminals and variables. *)
module Rules :
  sig
    type elt = string * string list * HotTerm.SortTerm.t
    type t
    val empty : t
    val is_empty : t -> bool
    val singleton : elt -> t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val compare_subset : t -> t -> int
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val pop : t -> elt * t
    val enum : t -> elt BatEnum.t
    val backwards : t -> elt BatEnum.t
    val of_enum : elt BatEnum.t -> t
    val print :
      ?first:string ->
      ?last:string ->
      ?sep:string ->
      ('a BatInnerIO.output -> elt -> unit) ->
      'a BatInnerIO.output -> t -> unit
    module Exceptionless :
      sig
        val min_elt : t -> elt option
        val max_elt : t -> elt option
        val choose : t -> elt option
      end
    module Labels :
      sig
        val iter : f:(elt -> unit) -> t -> unit
        val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
        val for_all : f:(elt -> bool) -> t -> bool
        val exists : f:(elt -> bool) -> t -> bool
        val map : f:(elt -> elt) -> t -> t
        val filter : f:(elt -> bool) -> t -> t
        val filter_map : f:(elt -> elt option) -> t -> t
        val partition : f:(elt -> bool) -> t -> t * t
      end

    val union_all : t list -> t
    val of_list : elt list -> t
    val as_list : t -> elt list

    (** Exception for fetch when rule is not found. *)
    exception No_suitable_rule_found

    (** Fetches a single rule for a given non-terminal symbol.
      If it does not exist, No_suitable_rule_found exception is thrown. *)
    val fetch : t -> string -> elt
  end


(** {2 Types and Creation} *)

(** The internal type of the HORS. *)
type t

(** The type of the ranked alphabet used internally. *)
type ra = HotRankedAlphabet.StringRankedAlphabet.t

(** The type of a symbol of a ranked alphabet used internally. *)
type raelt = HotRankedAlphabet.StringRankedAlphabet.elt

(** Create a new HORS by providing a ranked alphabet of
terminal symbols, a ranked alphabet of non-terminal symbols,
a set of rules, and a start symbol. *)
val create : ra -> ra -> Rules.t -> raelt -> t

(** {2 Predicates} *)

(** Checks wether the given HORS is safe according to the
syntactic definition of safety in e.g. Knapik, T. et al. Higher-Order
Pushdown Trees are easy. FoSSaCS'02. *)
val is_safe : t -> bool

(** Checks if the given HORS is well-formed, i.e. if
all (non-)terminal symbols are used wrt. to their specific
order. *)
val is_well_formed : t -> bool (* TODO Do this automatically on creation. *)

(** Checks if the given HORS is productive, i.e. if all non-terminals
finally produce a terminal. *)
val is_productive : t -> bool (* TODO Definition of productiveness for higher-order NTs *)

(** {2 Utilities} *)

(** Returns a human-readable representation from a given HORS. *)
val string_of : t -> string

(** {2 Evaluation } *)

(** Evaluates one step of a term at the non-terminal given by the path
according to the rules in the given HORS.  If the path does not point to a
non-terminal, or the non-terminal has no corresponding rule in the HORS,
the input term is returned without any changes.

Given the term [c F F], the path [c.1], and the rules [F -> B c], the
result is [c (B c) F].
*)
val step : t -> HotTerm.SortTerm.t -> HotPath.t -> HotTerm.SortTerm.t

(** Evaluates all non-terminals for one step. Kind of a breadth-first
evaluation on all non-terminal leaves of the given term. *)
val step_all : t -> HotTerm.SortTerm.t -> HotTerm.SortTerm.t

(** Evaluates all non-terminals until the first terminal appears.
  Requires all non-terminals to be productive, otherwise loop! *)
val step_all_until_terminal : t -> HotTerm.SortTerm.t -> HotTerm.SortTerm.t

module TermSet :
  sig
    type elt = HotTerm.SortTerm.t
    type t
    val empty : t
    val is_empty : t -> bool
    val singleton : elt -> t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val compare_subset : t -> t -> int
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val pop : t -> elt * t
    val enum : t -> elt BatEnum.t
    val backwards : t -> elt BatEnum.t
    val of_enum : elt BatEnum.t -> t
    val print :
      ?first:string ->
      ?last:string ->
      ?sep:string ->
      ('a BatInnerIO.output -> elt -> unit) ->
      'a BatInnerIO.output -> t -> unit
    module Exceptionless :
      sig
        val min_elt : t -> elt option
        val max_elt : t -> elt option
        val choose : t -> elt option
      end
    module Labels :
      sig
        val iter : f:(elt -> unit) -> t -> unit
        val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
        val for_all : f:(elt -> bool) -> t -> bool
        val exists : f:(elt -> bool) -> t -> bool
        val map : f:(elt -> elt) -> t -> t
        val filter : f:(elt -> bool) -> t -> t
        val filter_map : f:(elt -> elt option) -> t -> t
        val partition : f:(elt -> bool) -> t -> t * t
      end
    val union_all : t list -> t
    val of_list : elt list -> t
    val as_list : t -> elt list
    val default_print : ?sort:bool -> t -> string
  end


(** Produces all contexts of a given height in a HORS.
  Requires that all non-terminals are productive!. *)
val contexts : t -> int -> TermSet.t
