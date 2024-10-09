open Generator

let input_file = ref ""
let usage_msg = "LRGenerator -i <input_file>"
let anon_fun _ = ()

let spec_list = [
    ("-i", Arg.Set_string input_file, "What is the input file that should be used for the grammar")
]

let start_symbol = ref ""
let grammar = ref []
let terminals = ref Generator.StringSet.([ "EMPTY"; "EOS" ] |> of_list)
let non_terminals = ref Generator.StringSet.empty

let parse_file = fun () ->
    let get_element symbol =
        let split = String.split_on_char '\"' symbol in
        if List.length split = 1 then (
            non_terminals := Generator.StringSet.(!non_terminals |> add symbol);
            symbol
        ) else (
            let sym = List.hd (List.tl split) in
            terminals := Generator.StringSet.(!terminals |> add sym);
            sym
        )
    in
    let rec parse_entry (entry : string list) (acc : string list) : string list list =
        match entry with
        | [] -> [List.rev acc]
        | x::xs ->
            if x = "|" then
                let reverse = List.rev acc in
                reverse::parse_entry xs []
            else
                parse_entry xs ((get_element x)::acc)
    in

    let rec generate_productions (head : string) (productions : string list list) : (string * string list) list =
        match productions with
        | [] -> []
        | x::xs ->
            (head, x)::generate_productions head xs
    in

    let rec parse_entries entries =
        match entries with
        | [] -> ()
        | x::xs ->
            try
                let remaining = List.tl (List.tl x) in (* Remove -> *)
                let head = get_element (List.hd x) in
                let productions = parse_entry remaining [] in
                let generated_productions = generate_productions head productions in
                grammar := !grammar @ generated_productions;
                parse_entries xs
            with _ -> ()
    in

    let entire_file = In_channel.with_open_bin !input_file In_channel.input_all in
    let entries = String.split_on_char '\n' entire_file in
    let entries = List.map (String.split_on_char ' ') entries in
    start_symbol := get_element (List.hd (List.hd entries));
    parse_entries entries

let () =
    Arg.parse spec_list anon_fun usage_msg;

    parse_file ();

    let action_tbl, full_sets = Generator.generate_action_table !grammar !terminals !non_terminals in
    Generator.print_action_table !grammar action_tbl full_sets !terminals !non_terminals !start_symbol

(*
    Issues with Nullable generation
    Shifts wrong on non_terminals
*)
