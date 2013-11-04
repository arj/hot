open Batteries
open BatEnum.Infix

exception Path_not_found_in_term

module type S = sig
  type re

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

module Make = functor (RA : HotRankedAlphabet.S) -> struct
  type re = RA.elt
  type t = 
    | App of t * t list
    | Ctor of re
    | Var of string
    | Bottom

  let mkApp t ts = match t with
    | App(Ctor(s), []) -> App(Ctor(s),ts)
    | _ -> App(t,ts)

  let mkCtor re = mkApp (Ctor(re)) [] (* They are always wrapped in an app?*)

  let mkVar s = Var(s)

  let mkBottom = Bottom

  let mkAppCtor c ts = mkApp (mkCtor c) ts

  module Path = struct
    type termt = t
    type t =
      | Empty
      | Ele of re * int * t

    let epsilon = Empty

    let string_of ?(epsilon=false) p =
      let rec string_of' = function
        | Empty -> []
        | Ele(c,i,p') ->
            (c,i) :: string_of' p'
      in
        match p with
          | Empty when epsilon -> "{epsilon}"
          | _ ->
              let f (c,i) = Printf.sprintf "%s.%i" (RA.string_of_elt
                                                      ~show_type:false c) i in
              let ps = BatList.map f (string_of' p) in
                String.concat "." ps

    let rec append p1 p2 = match p1 with
      | Empty -> p2
      | Ele(c,i,p') -> Ele(c,i,append p' p2)

    let rec rev_append p1 p2 = match p1 with
      | Empty -> p2
      | Ele(c1,i1,p) -> rev_append p (Ele(c1,i1,p2))

    let reverse p = rev_append p Empty

    let is_empty = function
      | Empty -> true
      | Ele(_,_,_) -> false

    let rec subst_path path newterm term = match term,path with
      | Bottom,Empty -> newterm
      | Bottom,_ -> raise Path_not_found_in_term
      | Var(_),Empty -> newterm
      | Var(_),_ -> raise Path_not_found_in_term
      | App(Ctor(c), ts), Ele(d,idx,path') when c = d ->
          let len = List.length ts in
            if idx >= len then
              raise Path_not_found_in_term
            else
              begin
                let ts' = BatList.mapi
                            (fun i t -> if i = idx then subst_path path' newterm t else t)
                            ts
                in
                  App(Ctor(c), ts')
              end
      | Ctor(_), Empty -> newterm
      | App(_,_), Empty -> newterm
      | App(_,_), _
      | Ctor(_), _ -> raise Path_not_found_in_term


    let path term var =
      let rec path_inner path_so_far term : t option = match term with
        | Bottom -> None
        | App(App(t,_),_) -> path_inner path_so_far t
        | App(Var(x), _)
        | Var(x) -> if x = var then Some path_so_far else None
        | App(Ctor(c), ts) ->
            begin
              let new_path i = Ele(c,i,path_so_far) in
              let paths = List.mapi (fun i t -> path_inner (new_path i) t) ts in
              let paths_non_none = List.filter BatOption.is_some paths in
                match paths_non_none with
                  | [] -> None
                  | x :: _ -> x
            end
        | Ctor(_) -> None
        | App(_,_) -> None

      in
      let res = path_inner epsilon term in
        BatOption.map reverse res


    let rec read term path = match path with
      | Empty -> term
      | Ele(c,i,p') ->
          match term with
            | App(Ctor(ac),ts) when c = ac ->
                begin
                  try
                    read (List.nth ts i) p'
                  with
                    | Invalid_argument _ -> raise Path_not_found_in_term
                end
            | _ -> raise Path_not_found_in_term

    let rec length p = match p with
      | Empty -> 0
      | Ele(_,_,p') -> 1 + length p'

    let remove_suffix path suff =
      let path' = reverse path in
      let suff' = reverse suff in
      let rec inner p s = match p,s with
        | Empty, Ele(_,_,_) -> raise Path_not_found_in_term
        | p',Empty -> reverse p (* base *)
        | Ele(cp,ip,p'), Ele(cs,is,s') ->
            if cp = cs && ip = is then
              inner p' s'
            else
              raise Path_not_found_in_term
      in
        inner path' suff'

    let is_suffix p suff =
      try
        ignore @@ remove_suffix p suff;
        true
      with
        Path_not_found_in_term -> false

    module Infix = struct
      let (-->) = path
      let (-.) = read
    end
  end

  let rec string_of ?(show_type=false) = function
    | App(t,[]) -> Printf.sprintf "%s" (string_of ~show_type:show_type t)
    | App(t,ts) ->
        Printf.sprintf "%s(%s)"
          (string_of ~show_type:show_type t)
          (String.concat "," (List.map (string_of ~show_type:show_type) ts))
    | Ctor(re) -> RA.string_of_elt ~show_type:show_type re
    | Var(x) -> x
    | Bottom -> "_|_"

  let rec eta_reduce = function
    | App(App(Ctor(re), []),ts) -> App(Ctor(re),ts)
    | App(t1,ts) -> App(eta_reduce t1, List.map eta_reduce ts)
    | _ as t -> t

  let rec subst x newterm c = match c with
(*    | App(t,ts) -> eta_reduce (App(subst x newterm t, List.map (subst x newterm) ts))*)
    | App(t,ts) -> mkApp (subst x newterm t) (List.map (subst x newterm) ts)
    | Var(y) when x = y -> newterm 
    | Var(y) -> Var(y)
    | Ctor(c) -> Ctor(c)
    | Bottom -> Bottom

  let rec is_welldefined_list ts = List.fold_right (fun t ack -> ack && is_welldefined t) ts true

  and is_welldefined term = match term with
    | Bottom -> true
    | Var(_) -> true
    | App(Bottom,_) -> false
    | App(Var(_),ts) -> is_welldefined_list ts
    | App(Ctor(c),ts) ->
        if RA.arity_of_elt c == List.length ts
        then is_welldefined_list ts
        else false
    | _ -> false (* TODO Check *)

  let rec depth = function
    | Bottom -> 1
    | Var(_) -> 1
    | App(_,ts) ->
        let depths = List.map depth ts in
          List.max (0 :: depths) + 1
    | Ctor(_) -> failwith "May not occur"

  let compare = compare

  let vars term =
    let rec vars' p term = match term with
      | App(Ctor(s),ts) ->
          let p' i = Path.append p @@ Path.Ele(s,i,Path.Empty) in
          let is = BatList.of_enum @@ 0 --^ List.length ts in
          List.concat @@ List.map (fun (v,i) -> vars' (p' i) v) @@ List.combine ts is
      | App(_,_) -> failwith @@ "Not supported " ^ string_of term
      | Ctor(_) -> []
      | Var(x) -> [(x,p)]
      | Bottom -> []
    in
      vars' Path.Empty term

  open BatResult.Monad

  (* TODO Use proper monads, but the Batteries' Monads are unusable *)
  let rec unify t1 t2 : ((string * t) list, unit) BatResult.t = match t1,t2 with
    | App(t,ts),App(t',ts') ->
        begin
        match unify t t' with
          | Ok(u1) ->
              if BatList.length ts = BatList.length ts' then
                begin
                  let inner = BatList.map2 unify ts ts' in
                  let f ack e = match ack,e with
                    | Ok(lack), Ok(lst) -> return @@ lst @ lack
                    | Bad(t), _ -> Bad(t)
                    | _,Bad(t) -> Bad(t)
                  in
                    BatList.fold_left f (Ok(u1)) inner
                end
              else
                Bad(())
          | Bad(b) -> Bad(b)
        end
    | Ctor(c),Ctor(c') when c = c' -> return []
    | Bottom,Bottom -> return []
    | Var(x),_ -> return [(x,t2)]
    | _,Var(x) -> return [(x,t1)]
    | Bottom,_ -> return []
    | _,Bottom -> return []
    | _,_ -> Bad(())

  let rec context_unification (t1,p1) (t2,p2) : ((string * t) list, unit)
                                        BatResult.t =
    let open Path in
    let open Path.Infix in
    if not (is_suffix p1 p2) && not (is_suffix p2 p1) then
      Bad(())
    else
      if not (is_suffix p1 p2) then
        context_unification (t2,p2) (t1,p1)
      else
        begin
          (* We know that is_suffix p1 p2 holds, i.e.
             p1 = p. p2 *)
          let p = remove_suffix p1 p2 in
          let t1' = t1 -. p in
            unify t1' t2
        end
end
