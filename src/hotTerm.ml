open Batteries

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

  val string_of : ?sort:bool -> t -> string

  val eta_reduce : t -> t
  val subst : string -> t -> t -> t
  val subst_path : HotPath.t -> t -> t -> t
  val path : t -> string -> HotPath.t option
  val read : t -> HotPath.t -> t
  val is_welldefined : t -> bool
  val depth : t -> int

  module type Infix = sig
    val (-->) : t -> string -> HotPath.t option
    val (-.) : t -> HotPath.t -> t
  end
end

module Make = functor (RA : HotRankedAlphabet.S) -> struct
  type re = RA.elt
  type t = 
    | App of t * t list
    | Ctor of re
    | Var of string
    | Bottom

  let mkApp t ts = match t with
    | App(Ctor(s,tp), []) -> App(Ctor(s,tp),ts)
    | _ -> App(t,ts)

  let mkCtor re = Ctor(re(

  let mkVar s = Var(s)

  let mkBottom = Bottom

  let rec string_of ?(sort=false) = function
    | App(t,[]) -> Printf.sprintf "%s" (string_of ~sort:sort t)
    | App(t,ts) ->
        Printf.sprintf "%s(%s)"
          (string_of ~sort:sort t)
          (String.concat "," (List.map (string_of ~sort:sort) ts))
    | Ctor(re) -> RA.string_of_elt re
    | Var(x) -> x
    | Bottom -> "_|_"

  let rec eta_reduce = function
    | App(App(Ctor(re), []),ts) -> App(Ctor(re),ts)
    | App(t1,ts) -> App(eta_reduce t1, List.map eta_reduce ts)
    | _ as t -> t

  let rec subst x newterm c = match c with
    | App(t,ts) -> eta_reduce (App(subst x newterm t, List.map (subst x newterm) ts))
    | Var(y) when x = y -> newterm 
    | Var(_)
    | Ctor(_)
    | Bottom -> c

  let rec subst_path path newterm term = match term,path with
    | Bottom,HotPath.Empty -> newterm
    | Bottom,_ -> raise Path_not_found_in_term
    | Var(_),HotPath.Empty -> newterm
    | Var(_),_ -> raise Path_not_found_in_term
    | App(Ctor(c), ts), Path.Ele(d,idx,path') when c = d ->
        let len = List.length ts in
          if idx >= len then
            raise Path_not_found_in_term
          else
            begin
              let ts' = BatList.mapi
                          (fun i t -> if i = idx then subst_path path' newterm t else t)
                          ts
              in
                App(Ctor(c,st), ts')
            end
    | Ctor(_), HotPath.Empty -> newterm
    | App(_,_), HotPath.Empty -> newterm
    | App(_,_), _
    | Ctor(_), _ -> raise Path_not_found_in_term


  let path term var =
    let rec path_inner path_so_far term : HotPath.t option = match term with
      | Bottom -> None
      | App(App(t,_),_) -> path_inner path_so_far t
      | App(Var(x), _)
      | Var(x) -> if x = var then Some path_so_far else None
      | App(Ctor(c), ts) ->
          begin
            let new_path i = Path.Ele(c,i,path_so_far) in
            let paths = List.mapi (fun i t -> path_inner (new_path i) t) ts in
            let paths_non_none = List.filter BatOption.is_some paths in
              match paths_non_none with
                | [] -> None
                | x :: _ -> x
          end
      | Ctor(_) -> None
      | App(_,_) -> None

    in
    let res = path_inner Path.epsilon term in
      BatOption.map Path.reverse res


  let rec read term path = match path with
    | Path.Empty -> term
    | Path.Ele(c,i,p') ->
        match term with
          | App(Ctor(ac,_),ts) when c = ac ->
              begin
                try
                  read (List.nth ts i) p'
                with
                  | Invalid_argument _ -> raise Path_not_found_in_term
              end
          | _ -> raise Path_not_found_in_term

  let rec is_welldefined_list ts = List.fold_right (fun t ack -> ack && is_welldefined t) ts true

  and is_welldefined term = match term with
    | Bottom -> true
    | Var(_) -> true
    | App(Bottom,_) -> false
    | App(Var(_),ts) -> is_welldefined_list ts
    | App(Ctor(_,a),ts) ->
        if ASort.arity a == List.length ts
        then is_welldefined_list ts
        else false
    | _ -> false (* TODO Check *)

  let rec depth = function
    | Bottom -> 1
    | Var(_) -> 1
    | App(_,ts) ->
        let depths = List.map depth ts in
          List.max (0 :: depths) + 1
    | Ctor(_,_) -> failwith "May not occur"

  module Infix = struct
    let (-->) = path
    let (-.) = read
  end
end

module SortTerm = Make(HotType.Sort)
