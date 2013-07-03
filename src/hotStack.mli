(** In this module, a order-n stack (n-stack) with links is
  defined. For formal introduction, please see e.g. Maslov, A. N. - Multilevel
  stack automata.

  @author Robert Jakob
*)

(** The standard signature of a stack. *)
module type S = sig
  (* {1 Types } *)

  (** The internal type of a n-stack. *)
  type t

  (** The alphabet over which the stack is defined. *)
  type alphabet

  (** The type of a word of the alphabet. *)
  type word_of_alphabet

  (** The type of a symbol consists of the words of an alphabet, and two links
    as explained in Hague, M. et al. - Collapsible Pushdown Automata and
    Recursion Schemes. *)
  type symbol = word_of_alphabet * int * int

  (** Creates a new empty stack.
    @param k An optional order of the empty stack created. Defaults to 0. *)
  val create : ?k:int -> unit -> t

  (** Represents the distinguished bottom of stack symbol. *)
  val bottom_of_stack : word_of_alphabet

  (* {1 Operations} *)

  (** Returns the top (i-1)-stack of the given stack. *)
  val top : int -> t

  (** Pops the top (i-1)-stack. *)
  val pop : int -> t (*TODO Should this return a tuple? *)

  (** Pushs a symbol on top of the 1-stack of the given stack. *)
  val push_1_internal : t -> word_of_alphabet -> t

  (** Pushes a given symbol on top of the i-stack of the given stack. *)
  val push_1 : t -> word_of_alphabet -> int -> t

  (** Collapses the given stack to a prefix. *)
  val collapse : t -> t

  (** Returns the symbol when, the given stack is a order-0 stack.
    @raise Not_of_zeroth_order if [order t > 0]. *)
  val symbol : t -> symbol

  (* {1 Predicates} *)

  (** Checks if the given PDA produces the empty language
    when seen as a generator. *)
  val is_empty : t -> bool

  (** Returns the number of elements of the given stack, i.e.
    {v length [[a] [[a b] [c d]] v} would return 2. *)
  val length : t -> int

  (** Returns the number of elements in all levels, i.e.
    {v length [[a] [[a b] [c d]] v} would return 5. *)
  val elements : t -> int

  (** Returns the order of the stack, i.e. the 'n' of n-stack or the
    maximum number of nested stacks. *)
  val order : t -> int

  (** {1 Utilities} *)

  (** Creates a human-readable representation of the given stack. *)
  val string_of : t -> string
end
