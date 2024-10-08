module Generator = struct
    type item_T = int * int (* Rule Index, Offset *)

    module StringSet = Set.Make(String)
    module ItemSet = Set.Make(struct
        type t = item_T
        let compare = (fun (r1, i1) (r2, i2) ->
            let comp1 = compare r1 r2 in
            if comp1 = 0 then compare i1 i2 else comp1)
    end)

    type action_T = SHIFT of int
                  | REDUCE of int
                  | ACCEPT

    type rule_T = string * (string list) (* Nonterm, production *)
    type table_entry_T = int * string (* Rule, Input *)

    type action_table_T = (table_entry_T, action_T) Hashtbl.t
    type group_table_T = (ItemSet.t, int) Hashtbl.t

    type follow_table_T = (string, StringSet.t) Hashtbl.t

    type grammar_T = rule_T list

    let _print_table x =
        let lst = List.of_seq (Hashtbl.to_seq x) in
        List.iter (fun (sym, set) -> Printf.printf "%s: [ %s ]\n" sym (join_array (StringSet.(set |> to_list)))) lst

    let _track_error cmd =
        try
            cmd
        with e -> Printf.printf "Found Error\n"; raise e

    let string_of_action = function
        | SHIFT s -> Printf.sprintf "S%d" s
        | REDUCE r -> Printf.sprintf "R%d" r
        | ACCEPT -> Printf.sprintf "ACC"

    let rec take n x =
        match n, x with
        | 0, _ -> []
        | n, x::xs -> x::(take (n-1) xs)
        | _, [] -> []

    let rec drop n x =
        match n, x with
        | 0, x -> x
        | n, _::xs -> drop (n-1) xs
        | _, [] -> []

    let rec join_array = function
        | [] -> ""
        | x::xs -> Printf.sprintf "%s %s" x (join_array xs)

    let get_rule (grammar : grammar_T) ((rule, _) : item_T) : rule_T =
        List.nth grammar rule

    let after_dot (grammar : grammar_T) ((rule, index) : item_T) : string option =
        let _, rhs = get_rule grammar (rule, index) in
        List.nth_opt rhs index

    let print_item (grammar : grammar_T) ((rule, index) : item_T) : unit =
        let lhs, rhs = get_rule grammar (rule, index) in
        let head = if lhs = "" then "S" else lhs in
        let before_dot = join_array (take index rhs) in
        let after_dot = join_array (drop index rhs) in
        Printf.printf "%s -> %s∘%s\n" head before_dot after_dot

    let _print_itemset (grammar : grammar_T) (current_set_index : int) (itemset : ItemSet.t) : unit =
        let rec print_list = function
            | x::xs ->
                    Printf.printf "%d:\n" current_set_index;
                    print_item grammar x;
                    print_list xs
            | [] -> ()
        in
        let list = ItemSet.(itemset |> to_list) in
        print_list list; Printf.printf "\n"

    let generate_nullable_lookup (grammar : grammar_T) (terminals : StringSet.t) (non_terminals : StringSet.t) : (string, bool) Hashtbl.t =
        let null_lookup = Hashtbl.create 100 in

        let rec can_be_empty non_term lst =
            match lst with
            | [] -> false
            | (sym, prod)::gs -> (
                if sym = non_term then (
                    let hd = List.hd prod in
                    (* Printf.printf "%s -> %s\n" sym (join_array prod); *)
                    if StringSet.(terminals |> mem hd) then (
                        if hd = "EMPTY" then true
                        else can_be_empty non_term gs
                    ) else (
                        if hd = non_term then can_be_empty non_term gs
                        else (
                            let empties = List.map (fun x -> can_be_empty x grammar) prod in
                            List.fold_left (fun x y -> x && y) true empties
                        )
                    )
                ) else (
                    can_be_empty non_term gs
                )
            )
        in

        let rec is_nullable lst =
            match lst with
            | [] -> ()
            | x::xs -> (
                (* let empty = can_be_empty x grammar in *)
                let empty = false in
                Hashtbl.add null_lookup x empty;
                is_nullable xs
            )
        in

        let non_terminal_list = StringSet.(non_terminals |> to_list) in
        is_nullable non_terminal_list;
        null_lookup


    let check_is_nullable (terminals : StringSet.t) (null_lookup : (string, bool) Hashtbl.t) (symbol : string) : bool =
        if StringSet.(terminals |> mem symbol) then (
            if symbol = "EMPTY" then
                true
            else
                false
        ) else (
            Hashtbl.find null_lookup symbol
        )

    let generate_follow_table (grammar : grammar_T) (terminals : StringSet.t) (non_terminals : StringSet.t) : follow_table_T =
        let first_table = Hashtbl.create 100 in
        let follow_table = Hashtbl.create 100 in

        Printf.printf "Before nullable\n";
        let null_table = generate_nullable_lookup grammar terminals non_terminals in
        Printf.printf "After nullable\n";

        let rec initialize_first_table = function
            | [] -> ()
            | x::xs -> Hashtbl.add first_table x (StringSet.([ x ] |> of_list)); initialize_first_table xs
        in

        let rec initialize_follow_table = function
            | [] -> ()
            | x::xs -> Hashtbl.add follow_table x (StringSet.([ "EOS" ] |> of_list)); initialize_follow_table xs
        in

        let list_all_nullable (list : string list) =
            let nullable_list = List.map (fun x -> check_is_nullable terminals null_table x) list in
            List.fold_left (fun acc y -> acc && y) true nullable_list
        in

        let update_tables symbol prod =
            let i = ref 0 in
            let j = ref 0 in

            let has_changed = ref false in

            let set_size set = List.length (StringSet.elements set) in

            let check_change original_list new_list target_table target_symbol =
                if (set_size new_list) != (set_size original_list) then (
                    has_changed := true;
                    Hashtbl.replace target_table target_symbol new_list
                )
            in

            let get_set table symbol =
                try
                    Hashtbl.find table symbol
                with _ ->
                    StringSet.empty
            in

            let update_table table1 symbol1 table2 symbol2 target_table target_symbol =
                let original = get_set table1 symbol1 in
                let to_add = get_set table2 symbol2 in
                let new_list = StringSet.union original to_add in
                check_change original new_list target_table target_symbol;
            in

            let length = List.length prod in
            while !i < length do
                j := !i + 1;

                if !i = 0 || list_all_nullable (take !i prod) then (
                    update_table first_table symbol first_table (List.nth prod !i) first_table symbol
                );

                if (!i = (length - 1)) || list_all_nullable (drop !i prod) then (
                    update_table follow_table (List.nth prod !i) follow_table symbol follow_table (List.nth prod !i)
                );

                while !j < length do
                    if (!i + 1) = !j || list_all_nullable (take (!j - !i) (drop !i prod)) then (
                        update_table follow_table (List.nth prod !i) first_table (List.nth prod !j) follow_table (List.nth prod !i)
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
                let output = update_tables sym prod in
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

        initialize_first_table (StringSet.(terminals |> to_list));
        initialize_follow_table (StringSet.(non_terminals |> to_list));

        compute_sets ();
        remove_terminals follow_table;
        Printf.printf "\nFollow:\n";
        _print_table follow_table;
        follow_table

    let prod_closure (grammar : (string * string list) list) items =
        let rec find_rules grammar symbol index =
            match grammar with
            | (lhs, _)::gs ->
                if symbol = lhs then
                    (index, 0)::(find_rules gs symbol (index + 1))
                else
                    find_rules gs symbol (index + 1)
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
        let rec prod_closure_cache items cache =
            match items with
            | [] -> cache
            | i::is ->
                let sym = after_dot grammar i in
                match sym with
                | Some s ->
                    let to_add = find_rules grammar s 0 in
                    let new_items, new_cache = add_items_to_cache is cache to_add in
                    prod_closure_cache new_items new_cache
                | None ->
                    prod_closure_cache is cache
        in
        prod_closure_cache items (ItemSet.(items |> of_list))

    let partition_set (grammar : grammar_T) (items : ItemSet.t) =
        let rec partition_set_rec items hashtable =
            match items with
            | [] -> hashtable
            | i::is -> (
                let sym = after_dot grammar i in
                match sym with
                | Some _ ->
                    let rule, index = i in
                    let item = (rule, index + 1) in
                    let items = ItemSet.(prod_closure grammar [ item ] |> to_list) in
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
            (x, ItemSet.(y |> of_list))) list

    let generate_groups_and_action (grammar : grammar_T) (follow : follow_table_T) : (action_table_T * group_table_T) =
        let initial_set = prod_closure grammar [ (0, 0) ] in
        let first_element = (Some "", initial_set) in
        let added_groups = Hashtbl.create 100 in
        let next_set = ref 1 in
        Hashtbl.add added_groups initial_set 0;
        let action_tbl = Hashtbl.create 100 in (* (State, Symbol), ACTION Next_state *)

        let rec add_sets_to_table current_set (sets : (string option * ItemSet.t) list) =
            match sets with
            | [] -> []
            | (sym, set)::ss ->
                if (Hashtbl.mem added_groups set) then (
                    let existing_index = Hashtbl.find added_groups set in
                    let _ = match sym with
                          | None -> ()
                          | Some s -> Hashtbl.add action_tbl (current_set, s) (SHIFT existing_index)
                    in
                    []
                ) else (
                    let new_set = !next_set in
                    next_set := !next_set + 1;
                    Hashtbl.add added_groups set new_set;
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
                (* Printf.printf "\nGenerate_set_rec\n"; *)
                (* Printf.printf "List:\n"; *)
                (* List.iter (fun x -> print_item grammar x) ItemSet.(lst |> to_list); *)
                let new_sets = partition_set grammar lst in
                (* Printf.printf "Generated Sets: \n"; *)
                (* print_partition_set grammar current_set new_sets; *)
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
                | (state, _)::xs -> (
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
        add_reduce_to_action (List.of_seq (Hashtbl.to_seq added_groups));
        (action_tbl, added_groups)

    let generate_action_table (grammar : grammar_T) (terminals : StringSet.t) (non_terminals : StringSet.t) : action_table_T * group_table_T =
        let follow_table = generate_follow_table grammar terminals non_terminals in
        let action_tbl, full_groups = generate_groups_and_action grammar follow_table in
        (action_tbl, full_groups)

    let print_action_table (_grammar : grammar_T) (action_table : action_table_T) (_full_group : group_table_T) (terminals : StringSet.t) (non_terminals : StringSet.t) : unit =
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
            let print_item x =
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
        in

        let order_of_items = ((StringSet.(terminals |> to_list)) @ (StringSet.(non_terminals |> to_list))) in
        let ((max_group, _), _) = List.hd (List.sort (fun ((a, _), _) ((b, _), _) -> b - a) (List.of_seq (Hashtbl.to_seq action_table))) in

        Printf.printf "      |"; print_list order_of_items;
        print_items 0 max_group order_of_items
end
