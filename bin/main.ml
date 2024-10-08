type item_T = Item of int * int

let compare_Item a b =
    match (a, b) with
    | (Item (rule1, index1), Item (rule2, index2)) ->
        let comp1 = compare rule1 rule2 in
        if comp1 = 0 then (
            compare index1 index2
        ) else (
            comp1
        )

module ItemSet = Set.Make(struct
    type t = item_T
    let compare = compare_Item
end)

module StringSet = Set.Make(String)

(* module StringSet = Set.Make(struct *)
(*     type t = first_follow_T *)
(*     let compare = compare_First_Follow *)
(* end) *)

type action_T = SHIFT of int
              | REDUCE of int
              | ACCEPT

let string_of_action s =
    match s with
    | SHIFT s -> Printf.sprintf "S%d" s
    | REDUCE s -> Printf.sprintf "R%d" s
    | ACCEPT -> Printf.sprintf "ACC"

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

let non_terminals = StringSet.([ ""; "E"; "T" ] |> of_list)
let terminals = StringSet.([ "+"; "-"; "num"; "EMPTY"; "EOS" ] |> of_list)

(* let grammar = [ *)
(*     ("",  [ "E"; "EMPTY" ]); *)
(*     ("E", [ "s"; "EMPTY" ]) *)
(* ] *)
(**)
(* let non_terminals = StringSet.([ ""; "E" ] |> of_list) *)
(* let terminals = StringSet.([ "s"; "EMPTY" ] |> of_list) *)

let rec take n x =
    match (n, x) with
    | (0, _) -> []
    | (n, x::xs) -> x :: (take (n-1) xs)
    | (_, []) -> []

let rec drop n x =
    match (n, x) with
    | (0, x) -> x
    | (n, _::xs) -> (drop (n-1) xs)
    | (_, []) -> []

let rec join_array = function
    | x::xs ->
            Printf.sprintf "%s %s" x (join_array xs)
    | [] -> ""


let rec print_grammar grammar =
    match grammar with
    | (lhs,rhs)::xs ->
            (Printf.printf "%s -> %s\n" (if lhs = "" then "S" else lhs) (join_array rhs)); print_grammar xs
    | [] -> ()

let print_item grammar current_set (Item (rule, index)) =
    let lhs, rhs = List.nth grammar rule in
    Printf.printf "%d: %s -> %s∘%s\n" current_set (if lhs = "" then "S" else lhs) (join_array (take index rhs))  (join_array (drop index rhs))

let print_itemset grammar index itemSet =
    let rec print_list = function
        | x::xs -> print_item grammar index x; print_list xs
        | [] -> ()
    in
    let list = ItemSet.(itemSet |> to_list) in
    print_list list

let after_dot (grammar : (string * string list) list) (Item (rule, index)) =
    let _, rhs = List.nth grammar rule in
    List.nth_opt rhs index

let predict (grammar : (string * string list) list) items =
    let rec find_lhs grammar symbol index =
        match grammar with
        | (lhs, _)::gs ->
            if symbol = lhs then
                (Item (index, 0))::(find_lhs gs symbol (index + 1))
            else
                find_lhs gs symbol (index + 1)
        | [] -> []
    in
    let rec add_items_to_cache items cache to_add =
        match to_add with
        | [] -> (items, cache)
        | i::is ->
            if ItemSet.(cache |> mem i) then (
                add_items_to_cache items cache is
            ) else (
                add_items_to_cache (i :: items) (ItemSet.(cache |> add i)) is
            )
    in
    let rec predict_cache items cache =
        match items with
        | [] -> cache
        | i::is ->
            let sym = after_dot grammar i in
            match sym with
            | Some s ->
                (* Printf.printf "%s\n" s; *)
                let to_add = find_lhs grammar s 0 in
                let new_items, new_cache = add_items_to_cache is cache to_add in
                predict_cache new_items new_cache
            | None ->
                predict_cache is cache
    in
    predict_cache items (ItemSet.(items |> of_list))

let partition_set grammar items =
    let rec partition_set_rec items hashtable =
        match items with
        | [] -> hashtable
        | i::is -> (
            let sym = after_dot grammar i in
            match sym with
            | Some _ ->
                let Item (rule, index) = i in
                let item = Item (rule, index + 1) in
                let items = ItemSet.(predict grammar [ item ] |> to_list) in
                if (Hashtbl.mem hashtable sym) then (
                    Hashtbl.replace hashtable sym (items @ (Hashtbl.find hashtable sym));
                    partition_set_rec is hashtable
                ) else (
                    Hashtbl.add hashtable sym items;
                    partition_set_rec is hashtable
                )
            | None -> partition_set_rec is hashtable
            )
    in
    let hashtable = partition_set_rec (ItemSet.(items |> to_list)) (Hashtbl.create 5) in
    let list = List.of_seq (Hashtbl.to_seq hashtable) in
    List.map (fun (x, y) ->
        let z = List.map (fun x -> x) y in
        (x, ItemSet.(z |> of_list))) list

let rec print_partition_set grammar index (partition : ((string option * ItemSet.t) list)) =
    match partition with
    | [] -> ()
    | (x, set)::ps ->
        Printf.printf "%s\n" (match x with | Some s -> s | None -> "None");
        print_itemset grammar index set;
        print_partition_set grammar index ps

let generate_sets grammar (follow : (string, StringSet.t) Hashtbl.t) =
    let initial_set = predict grammar [ Item (0, 0) ] in
    let first_element = (Some "", initial_set) in
    let added_sets = Hashtbl.create 100 in
    let next_set = ref 1 in
    Hashtbl.add added_sets initial_set 0;
    let action_tbl = Hashtbl.create 100 in (* (State, Symbol), ACTION Next_state *)

    let rec add_sets_to_table current_set (sets : (string option * ItemSet.t) list) =
        match sets with
        | [] -> []
        | (sym, set)::ss ->
            if (Hashtbl.mem added_sets set) then (
                let existing_index = Hashtbl.find added_sets set in
                let _ = match sym with
                      | None -> ()
                      | Some s -> Hashtbl.add action_tbl (current_set, s) (SHIFT existing_index)
                in
                []
            ) else (
                let new_set = !next_set in
                next_set := !next_set + 1;
                Hashtbl.add added_sets set new_set;
                (* print_itemset grammar new_set set; *)
                let _ = match sym with
                      | Some s -> Hashtbl.add action_tbl (current_set, s) (SHIFT new_set)
                      | None -> ()
                in
                (new_set, (sym, set)) :: (add_sets_to_table current_set ss)
            )
    in
    let rec generate_set_rec current_sets =
        match current_sets with
        | [] -> ()
        | (current_set, (_, lst))::ss ->
            Printf.printf "\nGenerate_set_rec\n";
            Printf.printf "List:\n";
            List.iter (fun x -> print_item grammar 0 x) ItemSet.(lst |> to_list);
            let new_sets = partition_set grammar lst in
            Printf.printf "Generated Sets: \n";
            print_partition_set grammar current_set new_sets;
            let to_add_sets = add_sets_to_table current_set new_sets in
            let new_array = ss @ to_add_sets in

            generate_set_rec new_array
    in

    let rec add_reduce_to_action elements =
        let rec dot_at_end keys =
            match keys with
            | [] -> []
            | x::xs -> (
                let has_dot = after_dot grammar x in
                match has_dot with
                | None -> x :: dot_at_end xs
                | Some _ -> dot_at_end xs
            )
        in
        let rec add_reduce group elements =
            let rec add_reduce_to_state current_state reduce_state follow_states =
                match follow_states with
                | [] -> ()
                | x::xs ->
                        if reduce_state = 0 then (
                            Hashtbl.add action_tbl (current_state, x) (ACCEPT)
                        ) else (
                            Hashtbl.add action_tbl (current_state, x) (REDUCE reduce_state)
                        );
                        add_reduce_to_state current_state reduce_state xs
            in
            match elements with
            | [] -> ()
            | (Item (state, _))::xs -> (
                let (non_term, _) = List.nth grammar state in
                let reduce_states = Hashtbl.find follow non_term in
                add_reduce_to_state group state (StringSet.(reduce_states |> to_list));
                add_reduce group xs
            )
        in
        match elements with
        | [] -> ()
        | (set, group)::xs ->
            let lst = ItemSet.(set |> to_list) in
            let end_elements = dot_at_end lst in
            add_reduce group end_elements;
            add_reduce_to_action xs
    in

    generate_set_rec [ (0, first_element) ];
    add_reduce_to_action (List.of_seq (Hashtbl.to_seq added_sets));
    (action_tbl, added_sets)

(* let rec combine_non_terminals grammar x = *)
(*     match grammar with *)
(*     | (lhs, rhs)::xs -> *)
(*         if lhs = x then *)
(*             rhs :: combine_non_terminals xs x *)
(*         else *)
(*             combine_non_terminals xs x *)
(*     | [] -> [] *)

let get_nullable_table (grammar : (string * string list) list) =
    let null = Hashtbl.create 100 in

    let rec can_be_empty non_term lst =
        match lst with
        | [] -> false
        | (sym, prod)::gs ->
            if sym = non_term then (
                let hd = List.hd prod in
                (* Printf.printf "%s -> %s\n" sym (join_array prod); *)
                if StringSet.(terminals |> mem hd) then (
                    if hd = "EMPTY" then true
                    else can_be_empty non_term gs
                ) else (
                    if hd = non_term then
                        can_be_empty non_term gs
                    else (
                        let empties = List.map (fun x -> can_be_empty x grammar) prod in
                        List.fold_left (fun x y -> x && y) true empties
                    )
                )
            ) else (
                can_be_empty non_term gs
            )
    in

    let rec is_nullable lst =
        match lst with
        | [] -> ()
        | x::xs ->
            let empty = can_be_empty x grammar in
            Hashtbl.add null x empty;
            is_nullable xs
    in
    let non_terminal_list = StringSet.(non_terminals |> to_list) in
    is_nullable non_terminal_list;
    null

let check_is_nullable null sym =
    if StringSet.(terminals |> mem sym) then (
        if sym = "EMPTY" then
            true
        else
            false
    ) else (
        Hashtbl.find null sym
    )


let generate_first_follow grammar =
    let first = Hashtbl.create 100 in
    let follow = Hashtbl.create 100 in

    let null = get_nullable_table grammar in

    let rec initialize_first = function
        | [] -> ()
        | x::xs ->
                Hashtbl.add first x (StringSet.([ x ] |> of_list)); initialize_first xs
    in
    let rec initialize_follow = function
        | [] -> ()
        | x::xs ->
                Hashtbl.add follow x (StringSet.([ "EOS" ] |> of_list)); initialize_follow xs
    in
    let all_nullable lst =
        let nullable = List.map (fun x -> check_is_nullable null x) lst in
        List.fold_left (fun acc y -> acc && y) true nullable
    in
    let check_prod symbol prod =
        let i = ref 0 in
        let j = ref 0 in

        let has_changed = ref false in

        let set_size set = List.length (StringSet.elements set) in

        let length = List.length prod in
        while !i < length do
            j := !i + 1;

            if !i = 0 || all_nullable (take !i prod) then (
                let original = try Hashtbl.find first symbol with _ -> StringSet.empty in
                let to_add = try Hashtbl.find first (List.nth prod !i) with _ -> StringSet.empty in
                let new_list = StringSet.union original to_add in
                if (set_size new_list) != (set_size original) then (
                    has_changed := true;
                    Hashtbl.replace first symbol new_list
                )
            );

            if (!i = (length - 1)) || all_nullable (drop !i prod) then (
                let original = try Hashtbl.find follow (List.nth prod !i) with _ -> StringSet.empty in
                let to_add = try Hashtbl.find follow symbol with _ -> StringSet.empty in
                let new_list = StringSet.union to_add original in
                if (set_size new_list) != (set_size original) then (
                    has_changed := true;
                    Hashtbl.replace follow (List.nth prod !i) new_list
                    )
                );

            while !j < length do
                if (!i + 1) = !j || all_nullable (take (!j - !i) (drop !i prod)) then (
                    let original = try Hashtbl.find follow (List.nth prod !i) with _ -> StringSet.empty in
                    let to_add = try Hashtbl.find first (List.nth prod !j) with _ -> StringSet.empty in
                    let new_list = StringSet.union to_add original in
                    if (set_size new_list) != (set_size original) then (
                        has_changed := true;
                        Hashtbl.replace follow (List.nth prod !i) new_list
                    )
                );

                j := !j + 1;
            done;
            i := !i + 1;
        done;
        !has_changed
    in

    let rec check_grammar short_grammar =
        match short_grammar with
        | [] -> false
        | (sym, prod)::gs ->
            let output = check_prod sym prod in
            let next = check_grammar gs in
            output || next
    in

    let compute_sets = fun () ->
        let has_changed = ref true in
        while !has_changed do
            has_changed := check_grammar grammar
        done
    in

    let remove_terminals set =
        let comparator (key : string) value =
            if StringSet.(terminals |> mem key) then
                None
            else
                Some value
        in
        Hashtbl.filter_map_inplace comparator set
    in
    initialize_first (StringSet.(terminals |> to_list));
    initialize_follow (StringSet.(non_terminals |> to_list));
    (* first *)
    compute_sets ();
    remove_terminals follow;
    follow
    (* (remove_terminals first, remove_terminals follow) *)

let print_first_follow set =
    let rec print_set = function
        | [] -> ()
        | (symbol, set)::xs ->
            Printf.printf "%s : [%s]\n" symbol (join_array (StringSet.(set |> to_list)));
            print_set xs
    in
    let list = List.of_seq (Hashtbl.to_seq set) in
    print_set list

let print_full_sets grammar full_sets =
    let list = List.of_seq (Hashtbl.to_seq full_sets)in
    let sorted = List.sort (fun (_, a) (_, b) -> a - b) list in

    let rec print_set sets =
        match sets with
        | [] -> ()
        | (set, group)::xs ->
            print_itemset grammar group set;
            print_set xs
    in
    print_set sorted

let print_table action_table =
    let rec print_list = function
        | [] -> Printf.printf "\n"; ()
        | x::xs ->
                if (String.length x) = 0 then (
                    print_list xs
                ) else (
                    let str =
                        if x = "EOS" then "$"
                        else if x = "EMPTY" then "    󱃠"
                        else x
                    in
                    Printf.printf " %5s |" str; print_list xs
                )
    in
    let rec print_items current_group max_group full_items =
        let rec print_item x =
                let entry = Hashtbl.find_opt action_table (current_group, x) in
                if String.length x = 0 then
                    ()
                else (
                    (match entry with
                    | Some s ->
                            Printf.printf " %5s |" (string_of_action s);
                    | None ->
                            Printf.printf " %5s |" "";
                    );
                )
        in
        if current_group > max_group then
            ()
        else (
            Printf.printf "%5d |" current_group;
            List.iter (fun x -> print_item x) full_items;
            Printf.printf "\n";
            print_items (current_group + 1) max_group full_items
        )



        (* match current_items with *)
        (* | [] -> if current_group > max_group then *)
        (*             () *)
        (*         else (Printf.printf "\n"; print_items (current_group + 1) full_items max_group full_items) *)
        (* | x::xs -> *)
        (*         Printf.printf "%5d |" current_group; *)
        (*         let entry = Hashtbl.find_opt action_table (current_group, x) in *)
        (*         (match entry with *)
        (*         | Some _ -> *)
        (*                 Printf.printf " %5s | " "ADD"; *)
        (*         | None -> *)
        (*                 Printf.printf " %5s | " ""; *)
        (*         ); *)
        (*         print_items current_group xs max_group full_items *)
    in

    let order_of_items = ((StringSet.(terminals |> to_list)) @ (StringSet.(non_terminals |> to_list))) in
    let ((max_group, _), _) = List.hd (List.sort (fun ((a, _), _) ((b, _), _) -> b - a) (List.of_seq (Hashtbl.to_seq action_table))) in

    Printf.printf "      |"; print_list order_of_items;
    print_items 0 max_group order_of_items


let () =
    (* let set = [ Item (0, 0); Item (1, 0); Item (2, 0) ] |> ItemSet.of_list in *)
    (* print_grammar grammar; *)
    let follow = generate_first_follow grammar in
    let action_tbl, full_sets = generate_sets grammar follow in

    print_full_sets grammar full_sets;
    Printf.printf "\n";
    print_table action_tbl

    (* Partition generator wrong *)
