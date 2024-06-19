module StringMap = Map.Make(String);;

exception Number_Not_Found;;

let dico = StringMap.of_seq @@ List.to_seq [
    ("one",1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
    ("1",1);
    ("2", 2);
    ("3", 3);
    ("4", 4);
    ("5", 5);
    ("6", 6);
    ("7", 7);
    ("8", 8);
    ("9", 9);
  ];;

let get_lengths dico = 
  StringMap.fold 
  (fun key value acc -> let length = String.length key in 
    if List.exists (fun e -> e = length) acc
      then acc
      else length :: acc)
  dico []

let rec get_first_number_on_line str str_len index_start list_dictionary_key_sizes=
  if str_len <= index_start then raise Number_Not_Found
  else 
    let rec try_find_first_number lengths = 
      match lengths with
      |[] -> get_first_number_on_line str str_len (index_start + 1) list_dictionary_key_sizes
      | hd :: tl -> (try let s = String.sub str index_start hd in
                         StringMap.find s dico with
                        |Invalid_argument _
                        |Not_found -> try_find_first_number tl)
    in try_find_first_number list_dictionary_key_sizes

let rec get_last_number_on_line str str_len index_end list_dictionary_key_sizes=
  if index_end < 0 then raise Number_Not_Found
  else 
    let rec try_find_first_number lengths = 
      match lengths with
      |[] -> get_last_number_on_line str str_len (index_end -1) list_dictionary_key_sizes
      | hd :: tl -> (try let s = String.sub str (index_end - hd) hd in
                        StringMap.find s dico with
                        |Invalid_argument _
                        |Not_found -> try_find_first_number tl)
    in try_find_first_number list_dictionary_key_sizes

let get_number_of_line str list_dictionary_key_sizes=
  let length = String.length str in
  (get_first_number_on_line str length 0 list_dictionary_key_sizes) * 10 + get_last_number_on_line str length length list_dictionary_key_sizes

let () =
  if (Array.length Sys.argv != 2) 
    then 
      let () = print_string "Error: Missing filename argument in command call" in
      print_newline ()
    else 
      let file = Sys.argv.(1) in
      let list_dictionary_key_sizes = get_lengths dico in
      let lines = In_channel.input_lines (In_channel.open_bin file) in 
      let total = List.fold_left (fun acc x -> acc + (get_number_of_line x list_dictionary_key_sizes)) 0 lines in 
      let () = print_int total in print_newline ();;