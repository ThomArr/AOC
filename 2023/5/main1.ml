let debug = false

type infos = {source_range_start : int; source_range_end : int; difference:int}

module Infos = struct
  type t = infos
  let compare i1 i2 =
    let res = compare i1.source_range_start i2.source_range_start in
    if res <> 0 then res
    else
      let res = compare i1.source_range_end i2.source_range_end in
      if res <> 0 then res
      else
        compare i1.difference i2.difference 
end

module InfosSet = Set.Make(Infos);;
module IntSet = Set.Make(Int);;

let print_intset intset =
  IntSet.iter (fun x -> print_int x; print_string " ";) intset;
  print_newline ()

let print_int_list list =
  List.iter (fun x -> print_int x; print_string " ";) list;
  print_newline ()

let print_infos infos =
  match infos with
  |{source_range_start; source_range_end ; difference} -> print_string "{"; print_int source_range_start; print_string ";"; print_int source_range_end; print_string ";"; print_int difference; print_string "} "

let print_infosset infosset =
  InfosSet.iter (fun info -> print_infos info) infosset; print_newline ()

let print_InfosSet_list list =
  List.iter (fun infosset -> print_infosset infosset) list;
  print_newline ()

exception Invalid_input

let rec get_seeds str strlen index length list=
  if index+length = strlen 
    then 
      int_of_string (String.sub str index (length)) :: list
    else 
      match str.[index + length-1] with
      | n when '0' <= n && n <='9' -> get_seeds str strlen index (length+1) list
      | ' ' -> 
        get_seeds str strlen (index+length) 1 (int_of_string (String.sub str index (length-1)) :: list)
      | _ -> raise Invalid_input

let get_infos str=
  let strlen = String.length str in 
  let infos_brut =
    let rec get_infos_brut index length list =
      if index+length = strlen 
        then 
          int_of_string (String.sub str index (length)) :: list
        else 
          match str.[index + length-1] with
          | n when '0' <= n && n <='9' -> get_infos_brut index (length+1) list
          | ' ' -> get_infos_brut (index+length) 1 (int_of_string (String.sub str index (length-1)) :: list)
          | _ -> raise Invalid_input
    in get_infos_brut 0 1 []
  in match infos_brut with
  | [rl ; srs; drs] -> {source_range_start = srs; source_range_end = srs + rl - 1; difference = drs - srs}
  | _ -> raise Invalid_input

let infosSet_find_first_opt f infosSet =
  InfosSet.fold (fun infos acc -> if f infos then Some infos else acc) infosSet None

let rec get_all_localisation seed_list all_categories set_localisation=
  match seed_list with
  | hd :: tl ->
    if debug then (print_string "Seed: ";);
    get_all_localisation tl all_categories 
    (IntSet.add 
      (let res = List.fold_left 
        (fun acc categorie -> 
          match (infosSet_find_first_opt (fun infos -> infos.source_range_start <= acc && acc <= infos.source_range_end) categorie) with 
          | None -> if debug then (print_int acc; print_string" ";); acc 
          | Some info -> if debug then (print_int info.source_range_start; print_string "<="; print_int acc; print_string "<="; print_int info.source_range_end; print_string" "); info.difference + acc) 
        hd all_categories in 
        if debug then print_newline (); 
        res)
      set_localisation) 
  | [] -> set_localisation

let rec split_file filename=
  let lines = In_channel.input_lines (In_channel.open_bin filename) in 
  let seed_list_string = List.hd lines in
  let seed_list_string = String.sub seed_list_string 7 ((String.length seed_list_string) - 7) in
  let seed_list = get_seeds seed_list_string (String.length seed_list_string) 0 1 [] in
  if debug then print_int_list seed_list;
  let rec split_all_categories lines categories =
    match lines with
    | "" :: tl -> categories :: (split_all_categories (List.tl tl) InfosSet.empty)
    | hd :: tl -> split_all_categories tl (InfosSet.add (get_infos hd) categories)
    | [] -> [categories]
  in let all_categories = split_all_categories (List.tl lines) InfosSet.empty in
  if debug then print_InfosSet_list all_categories;
  let res = get_all_localisation seed_list all_categories IntSet.empty in
  if debug then print_intset res;
  IntSet.min_elt res

let () =
  if (Array.length Sys.argv != 2) 
    then 
      let () = print_string "Error: Missing filename argument in command call" in
      print_newline ()
    else 
      let () = print_int (split_file Sys.argv.(1)) in print_newline ();;
