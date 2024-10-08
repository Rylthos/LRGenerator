
module Generator : sig
    type item_T = int * int (* Rule Index, Offset *)

    module StringSet : (Set.S with type elt = string)

    module ItemSet : (Set.S with type elt = item_T)
    (* module ItemSet : Set.Make(struct *)
    (*     type t = item_T *)
    (*     let compare = (fun (r1, i1) (r2, i2) -> *)
    (*         let comp1 = compare r1 r2 in *)
    (*         if comp1 = 0 then compare i1 i2 else comp1) *)
    (* end) *)

    type action_T = SHIFT of int
                  | REDUCE of int
                  | ACCEPT

    type rule_T = string * (string list) (* Nonterm, production *)
    type table_entry_T = int * string (* Rule, Input *)

    type action_table_T = (table_entry_T, action_T) Hashtbl.t
    type group_table_T = (ItemSet.t, int) Hashtbl.t

    type follow_table_T = (string, StringSet.t) Hashtbl.t

    type grammar_T = rule_T list

    val generate_action_table : grammar_T -> StringSet.t -> StringSet.t -> (action_table_T * group_table_T)

    val print_action_table : grammar_T -> action_table_T -> group_table_T -> StringSet.t -> StringSet.t -> unit
end
