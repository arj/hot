open HotSimple
module SRA = StringRankedAlphabet

module NBA : HotTreeAutomaton.S = struct

  module Rules : HotTreeAutomaton.RULES
       with type symbol = SRA.elt = struct
    include HotExtBatSet.Make(
    struct 
      type t = string * SRA.elt * string list
      let compare = compare
    end)

    type symbol = SRA.elt
    type state = string

    let string_of_elt (q,a,qs) =
      Printf.sprintf "%s,%s -> %s"
        q
        (SRA.string_of_elt a)
        (String.concat " " qs)

    let string_of rules =
      let io = BatIO.output_string () in
        print ~first:"{" ~last:"}" ~sep:","
          (fun out r -> BatIO.nwrite out (string_of_elt r)) io rules;
        BatIO.close_out io
  end

  include HotTreeAutomaton.Make(SRA)(Rules)
end
