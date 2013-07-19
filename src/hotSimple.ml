
module StringRankedAlphabet : HotRankedAlphabet.S with type elt = string *
                                                 HotType.Sort.t =
  HotRankedAlphabet.Make(HotHelper.HotExtBatString)(HotType.Sort)

module StringSortTerm = HotTerm.Make(StringRankedAlphabet)

module StringSortPMRS =
  HotPMRS.Make(StringRankedAlphabet)(StringRankedAlphabet)(StringSortTerm)

let epsilon = "e"

module SimpleState : HotState.S with type t = string =
  HotState.Make(
    struct
      type t = string
      let create s = s
      let compare = String.compare
      let string_of t = t
      let of_string s = s
      let subst qold qnew q = if q = qold then qnew else q
      let epsilon = epsilon
    end)

(*module PathState = struct
  include Make(
    struct
      type t = string * RtaTerm.Path.t
      let create s = s
      let compare = compare
      let string_of (pre,p) = pre ^ ":" ^ (RtaTerm.Path.string_of ~epsilon:false p)
      let from_string _ = failwith "Not implemented"
      let subst qold qnew q = if q = qold then qnew else q
      let epsilon = ("",RtaTerm.Path.epsilon)
    end)

  let append (pre,p1) p2 =
      (pre, RtaTerm.Path.append p1 p2)
end*)

module SimpleStateSet = HotStateSet.Make(SimpleState)
(*module PathStateSet = Make(RtaState.PathState)*)
