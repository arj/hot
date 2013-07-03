type t =
  | Empty
  | Ele of string * int * t

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
          let f (c,i) = Printf.sprintf "%s.%i" c i in
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
