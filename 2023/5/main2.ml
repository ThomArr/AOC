let debug = true

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

let print_int_int_list list =
  List.iter (fun (x,y) -> print_string "["; print_int x; print_string " ;"; print_int y; print_string "]"; print_string " ";) list;
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

let rec apply_info_to_interval (lower_edge, upper_edge) info =
  if info.source_range_start <= lower_edge && lower_edge <= info.source_range_end
    then 
      if upper_edge <= info.source_range_end 
        then
          [(lower_edge+info.difference, upper_edge+info.difference)]
        else 
          [(lower_edge+info.difference, info.source_range_end+info.difference); (info.source_range_end + 1, upper_edge)]
    else 
      if info.source_range_start <= upper_edge && upper_edge <= info.source_range_end
        then 
          [(lower_edge, info.source_range_start - 1); (info.source_range_start+info.difference, upper_edge+info.difference)]
        else 
          if lower_edge < info.source_range_start && info.source_range_end < upper_edge
            then
              [(lower_edge, info.source_range_start - 1); (info.source_range_start + info.difference, info.source_range_end + info.difference); (info.source_range_end + 1, upper_edge)]
            else
              [(lower_edge, upper_edge)]
    
  
let rec get_all_new_intervals_using_this_categorie seed_interval_list categorie =
  match seed_interval_list with
  | (hd1, hd2) :: tl -> InfosSet.fold (fun info acc1 -> List.fold_left (fun acc2 x -> (apply_info_to_interval x info) @ acc2) [] acc1) categorie [(hd1, hd2)] @ get_all_new_intervals_using_this_categorie tl categorie
  | [] -> []

let list_min_start_interval list =
  let rec aux list acc =
    match  list with
    | (hd1,hd2) :: tl -> if hd1 < acc then aux tl hd1 else aux tl acc
    | [] -> Some acc
  in match list with
  | [] -> None
  | (hd1,hd2) :: tl -> aux tl hd1

exception Empty_list

let rec get_min_localisation seed_interval_list all_categories : int=
  let res = List.fold_left 
    (fun acc categorie -> 
      let res = get_all_new_intervals_using_this_categorie acc categorie in
      let () = if debug then print_int_int_list res in
      res)
    seed_interval_list all_categories in 
  if debug then print_newline (); 
  match list_min_start_interval res with 
  |None -> raise Empty_list 
  |Some e -> e

let rec init_seed seed_list =
  match seed_list with
  | a :: (b :: c) when a < 0 -> raise Invalid_input
  | a :: (b :: c) when a = 0-> init_seed c
  | a :: (b :: c) -> (b, b+a-1) :: init_seed c
  | [] -> []
  | _ -> raise Invalid_input

let rec split_file filename=
  let lines = In_channel.input_lines (In_channel.open_bin filename) in 
  let seed_list_string = List.hd lines in
  let seed_list_string = String.sub seed_list_string 7 ((String.length seed_list_string) - 7) in
  let seed_interval_list = get_seeds seed_list_string (String.length seed_list_string) 0 1 [] in
  let seed_interval_list = init_seed seed_interval_list in
  if debug then print_int_int_list seed_interval_list;
  let rec split_all_categories lines categories =
    match lines with
    | "" :: tl -> categories :: (split_all_categories (List.tl tl) InfosSet.empty)
    | hd :: tl -> split_all_categories tl (InfosSet.add (get_infos hd) categories)
    | [] -> [categories]
  in let all_categories = split_all_categories (List.tl lines) InfosSet.empty in
  if debug then print_InfosSet_list all_categories;
  get_min_localisation seed_interval_list all_categories

let () =
  if (Array.length Sys.argv != 2) 
    then 
      let () = print_string "Error: Missing filename argument in command call" in
      print_newline ()
    else 
      let () = print_int (split_file Sys.argv.(1)) in print_newline ();;
