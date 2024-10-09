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
    ("E", [ "F" ]);
    ("E", [ "E"; "+"; "F" ]);
    ("E", [ "E"; "-"; "F" ]);
    ("F", [ "T"; "^"; "F" ]);
    ("F", [ "T" ]);
    ("T", [ "trig"; "T" ]);
    ("T", [ "-"; "T" ]);
    ("T", [ "G" ]);
    ("G", [ "G"; "!" ]);
    ("G", [ "H" ]);
    ("H", [ "n" ])
]

let non_terminals = Generator.StringSet.([ ""; "E"; "F"; "T"; "G"; "H" ] |> of_list)
let terminals = Generator.StringSet.([ "+"; "-"; "^"; "trig"; "!"; "n"; "EOS" ] |> of_list)

(* let grammar = [ *)
(*     ("",  [ "A" ]); *)
(*     ("A", [ "B"; "C"; "D" ]); *)
(*     ("B", [ "a" ]); *)
(*     ("C", [ "+" ]); *)
(*     ("C", [ "EMPTY" ]); *)
(*     ("D", [ "b" ]) *)
(* ] *)
(**)
(* let non_terminals = Generator.StringSet.([ ""; "A"; "B"; "C"; "D" ] |> of_list) *)
(* let terminals = Generator.StringSet.([ "a"; "+"; "b"; "EMPTY"; "EOS" ] |> of_list) *)

let () =
    let action_tbl, full_sets = Generator.generate_action_table grammar terminals non_terminals in
    Generator.print_action_table grammar action_tbl full_sets terminals non_terminals;
    ()

(*
    Issues with Nullable generation
    Shifts wrong on non_terminals
*)
