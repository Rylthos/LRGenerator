open Generator

(*
    E' -> E
    E -> E + T
    E -> E - T
    E -> T
    T -> number
*)

let grammar = [
    ("",  [ "E" ]);
    ("E", [ "E"; "+"; "T" ]);
    ("E", [ "E"; "-"; "T" ]);
    ("E", [ "T" ]);
    ("T", [ "num" ])
]

let non_terminals = Generator.StringSet.([ ""; "E"; "T" ] |> of_list)
let terminals = Generator.StringSet.([ "+"; "-"; "num"; "EMPTY"; "EOS" ] |> of_list)

(* let rec print_partition_set grammar index (partition : ((string option * ItemSet.t) list)) = *)
(*     match partition with *)
(*     | [] -> () *)
(*     | (x, set)::ps -> *)
(*         Printf.printf "%s\n" (match x with | Some s -> s | None -> "None"); *)
(*         print_itemset grammar index set; *)
(*         print_partition_set grammar index ps *)
(**)
(**)
(* let print_first_follow set = *)
(*     let rec print_set = function *)
(*         | [] -> () *)
(*         | (symbol, set)::xs -> *)
(*             Printf.printf "%s : [%s]\n" symbol (join_array (StringSet.(set |> to_list))); *)
(*             print_set xs *)
(*     in *)
(*     let list = List.of_seq (Hashtbl.to_seq set) in *)
(*     print_set list *)
(**)
(* let print_full_sets grammar full_sets = *)
(*     let list = List.of_seq (Hashtbl.to_seq full_sets)in *)
(*     let sorted = List.sort (fun (_, a) (_, b) -> a - b) list in *)
(**)
(*     let rec print_set sets = *)
(*         match sets with *)
(*         | [] -> () *)
(*         | (set, group)::xs -> *)
(*             print_itemset grammar group set; *)
(*             print_set xs *)
(*     in *)
(*     print_set sorted *)



let () =
    let action_tbl, full_sets = Generator.generate_action_table grammar terminals non_terminals in
    Generator.print_action_table grammar action_tbl full_sets terminals non_terminals;
    ()
